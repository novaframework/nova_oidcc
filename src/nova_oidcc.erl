-module(nova_oidcc).

-export([load_user_info/1]).

-include_lib("oidcc/include/oidcc_token_introspection.hrl").

load_user_info(#{oidcc_extract_authorization := undefined}) ->
    false;
load_user_info(#{oidcc_extract_authorization := Token} = Req) ->
    #{
        provider := Provider,
        client_id := ClientId,
        client_secret := ClientSecret
    } =
        Opts =
        case application:get_env(nova_oidcc, oidcc_load_userinfo) of
            {ok,
                #{
                    provider := _Provider,
                    client_id := _ClientId,
                    client_secret := _ClientSecret
                } = Opts2} ->
                Opts2;
            _ ->
                erlang:error(no_config_provided)
        end,
    UserinfoRetrieveOpts0 = maps:get(userinfo_retrieve_opts, Opts, #{}),
    UserinfoRetrieveOpts = maps:put(expected_subject, any, UserinfoRetrieveOpts0),
    Cache = maps:get(cache, Opts, ?MODULE),

    case Cache:get(userinfo, Token, Req) of
        {ok, #{} = Claims} ->
            introspect_token(Req#{oidcc_load_userinfo => Claims});
        miss ->
            case
                oidcc:retrieve_userinfo(
                    Token, Provider, ClientId, ClientSecret, UserinfoRetrieveOpts
                )
            of
                {ok, #{} = Claims} ->
                    Cache:put(userinfo, Token, Claims, Req),
                    introspect_token(Req#{oidcc_load_userinfo => Claims});
                {error, {http_error, 401, _Body}} ->
                    {error, inactive_token, maps:put(?MODULE, undefined, Req)};
                {error, Reason} ->
                    erlang:error(Reason)
            end
    end;
load_user_info(_) ->
    erlang:error(no_oidcc_extract_authorization).

introspect_token(#{oidcc_extract_authorization := undefined}) ->
    false;
introspect_token(#{oidcc_extract_authorization := Token} = Req) ->
    #{
        provider := Provider,
        client_id := ClientId,
        client_secret := ClientSecret
    } =
        Opts =
        case application:get_env(nova_oidcc, oidcc_introspect_token) of
            {ok,
                #{
                    provider := _Provider,
                    client_id := _ClientId,
                    client_secret := _ClientSecret
                } = Opts2} ->
                Opts2;
            _ ->
                erlang:error(no_config_provided)
        end,
    TokenIntrospectionOpts = maps:get(token_introspection_opts, Opts, #{}),

    Cache = maps:get(cache, Opts, ?MODULE),

    case Cache:get(introspection, Token, Req) of
        {ok, #oidcc_token_introspection{active = true} = Introspection} ->
            validate_jwt_token(Req#{oidcc_introspect_token => Introspection});
        {ok, #oidcc_token_introspection{active = false} = Introspection} ->
            {error, inactive_token, Req#{oidcc_introspect_token => Introspection}};
        miss ->
            case
                oidcc:introspect_token(
                    Token, Provider, ClientId, ClientSecret, TokenIntrospectionOpts
                )
            of
                {ok, #oidcc_token_introspection{active = true} = Introspection} ->
                    Cache:put(introspection, Token, Introspection, Req),
                    validate_jwt_token(Req#{oidcc_introspect_token => Introspection});
                {ok, #oidcc_token_introspection{active = false} = Introspection} ->
                    {error, inactive_token, Req#{oidcc_introspect_token => Introspection}};
                {error, Reason} ->
                    erlang:error(Reason)
            end
    end;
introspect_token(#{} = _Req) ->
    erlang:error(no_oidcc_extract_authorization).

validate_jwt_token(#{oidcc_extract_authorization := undefined}) ->
    false;
validate_jwt_token(#{oidcc_extract_authorization := Token} = Req) ->
    #{
        provider := Provider,
        client_id := ClientId,
        client_secret := ClientSecret
    } =
        case application:get_env(nova_oidcc, oidcc_introspect_token) of
            {ok,
                #{
                    provider := _Provider,
                    client_id := _ClientId,
                    client_secret := _ClientSecret
                } = Opts2} ->
                Opts2;
            _ ->
                erlang:error(no_config_provided)
        end,
    case oidcc_client_context:from_configuration_worker(Provider, ClientId, ClientSecret) of
        {ok, ClientContext} ->
            case oidcc_token:validate_id_token(Token, ClientContext, any) of
                {ok, Claims} ->
                    {ok, Req#{oidcc_validate_jwt_token => Claims}};
                {error, token_expired} ->
                    {error, inactive_token, maps:put(?MODULE, undefined, Req)};
                {error, Reason} ->
                    erlang:error(Reason)
            end;
        {error, token_expired} ->
            {error, inactive_token, maps:put(?MODULE, undefined, Req)};
        {error, Reason} ->
            erlang:error(Reason)
    end;
validate_jwt_token(#{}) ->
    erlang:error(no_oidcc_cowboy_extract_authorization).
