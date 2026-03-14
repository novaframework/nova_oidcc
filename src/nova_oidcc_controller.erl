-module(nova_oidcc_controller).
-moduledoc """
Nova controller for OpenID Connect authentication flows.

Handles the login redirect and OAuth callback endpoints.

## Routes

```erlang
{"/:provider/login", fun nova_oidcc_controller:login/1, #{}}
{"/:provider/callback", fun nova_oidcc_controller:callback/1, #{}}
```
""".

-export([login/1, callback/1]).

-doc """
Initiate OIDC login by redirecting to the provider's authorization endpoint.

Generates nonce and PKCE verifier, stores them in the session,
and redirects the user to the provider.
""".
-spec login(map()) -> term().
login(Req = #{bindings := #{provider := ProviderBin}}) ->
    Provider = binary_to_existing_atom(ProviderBin, utf8),
    case nova_oidcc:provider_config(Provider) of
        {ok, #{client_id := ClientId, client_secret := ClientSecret}} ->
            Nonce = generate_random(32),
            PkceVerifier = generate_random(32),
            RedirectUri = nova_oidcc:redirect_uri(Provider),
            Scopes = application:get_env(nova_oidcc, scopes, [
                <<"openid">>, <<"profile">>, <<"email">>
            ]),

            ok = nova_session:set(Req, <<"oidcc_nonce">>, Nonce),
            ok = nova_session:set(Req, <<"oidcc_pkce">>, PkceVerifier),
            ok = nova_session:set(Req, <<"oidcc_provider">>, ProviderBin),

            ProviderWorker = nova_oidcc:provider_name(Provider),
            case
                oidcc:create_redirect_url(
                    ProviderWorker,
                    ClientId,
                    ClientSecret,
                    #{
                        redirect_uri => RedirectUri,
                        scopes => Scopes,
                        nonce => Nonce,
                        pkce_verifier => PkceVerifier
                    }
                )
            of
                {ok, AuthUrl} ->
                    {redirect, AuthUrl};
                {error, Reason} ->
                    handle_failure(Req, Reason)
            end;
        {error, not_found} ->
            {status, 404, #{}, <<"Unknown provider">>}
    end.

-doc """
Handle the OAuth callback after the user authenticates with the provider.

Exchanges the authorization code for tokens, retrieves user info,
stores the user in the session, and redirects to the success URL.
""".
-spec callback(map()) -> term().
callback(Req = #{bindings := #{provider := ProviderBin}}) ->
    Provider = binary_to_existing_atom(ProviderBin, utf8),
    QsList = cowboy_req:parse_qs(Req),
    case proplists:get_value(<<"code">>, QsList) of
        undefined ->
            ErrorDesc = proplists:get_value(<<"error_description">>, QsList, <<"no code">>),
            handle_failure(Req, {missing_code, ErrorDesc});
        Code ->
            handle_code(Req, Provider, Code)
    end.

%%----------------------------------------------------------------------
%% Internal
%%----------------------------------------------------------------------

handle_code(Req, Provider, Code) ->
    case nova_oidcc:provider_config(Provider) of
        {ok, #{client_id := ClientId, client_secret := ClientSecret}} ->
            {ok, Nonce} = nova_session:get(Req, <<"oidcc_nonce">>),
            PkceVerifier =
                case nova_session:get(Req, <<"oidcc_pkce">>) of
                    {ok, V} -> V;
                    _ -> none
                end,
            RedirectUri = nova_oidcc:redirect_uri(Provider),
            ProviderWorker = nova_oidcc:provider_name(Provider),

            TokenOpts = #{
                redirect_uri => RedirectUri,
                nonce => Nonce,
                pkce_verifier => PkceVerifier
            },

            case
                retrieve_token_and_userinfo(ProviderWorker, ClientId, ClientSecret, Code, TokenOpts)
            of
                {ok, Token, Userinfo} ->
                    %% Clean up auth state from session
                    nova_session:delete(Req, <<"oidcc_nonce">>),
                    nova_session:delete(Req, <<"oidcc_pkce">>),
                    nova_session:delete(Req, <<"oidcc_provider">>),

                    %% Store user in session (serialize to binary for nova_session)
                    ok = nova_session:set(Req, <<"oidcc_user">>, term_to_binary(Userinfo)),
                    ok = nova_session:set(
                        Req, <<"oidcc_token">>, term_to_binary(token_to_map(Token))
                    ),

                    %% Redirect to success URL
                    case application:get_env(nova_oidcc, on_success, {redirect, <<"/">>}) of
                        {redirect, Url} -> {redirect, Url};
                        _ -> {redirect, <<"/">>}
                    end;
                {error, Reason} ->
                    handle_failure(Req, Reason)
            end;
        {error, not_found} ->
            {status, 404, #{}, <<"Unknown provider">>}
    end.

retrieve_token_and_userinfo(ProviderWorker, ClientId, ClientSecret, Code, TokenOpts) ->
    case oidcc:retrieve_token(Code, ProviderWorker, ClientId, ClientSecret, TokenOpts) of
        {ok, Token} ->
            case oidcc:retrieve_userinfo(Token, ProviderWorker, ClientId, ClientSecret, #{}) of
                {ok, Userinfo} ->
                    {ok, Token, Userinfo};
                {error, _} ->
                    %% Userinfo optional — use ID token claims
                    {ok, Token, #{}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

token_to_map(Token) ->
    %% Extract serializable parts from the oidcc_token record
    #{
        access => element(2, Token),
        id => element(3, Token),
        refresh => element(4, Token)
    }.

handle_failure(_Req, Reason) ->
    logger:error("nova_oidcc authentication failed: ~p", [Reason]),
    case application:get_env(nova_oidcc, on_failure, {status, 401}) of
        {status, Code} -> {status, Code};
        {redirect, Url} -> {redirect, Url};
        _ -> {status, 401}
    end.

generate_random(Bytes) ->
    base64:encode(crypto:strong_rand_bytes(Bytes), #{mode => urlsafe, padding => false}).
