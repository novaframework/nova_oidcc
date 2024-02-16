-module(nova_oidcc).

-export([is_authorized/1]).
-export([get/3]).
-export([put/4]).

-include_lib("oidcc/include/oidcc_token_introspection.hrl").

get(_Type, _Token, _Req) -> miss.

put(_Type, _Token, _Data, _Req) -> ok.
is_authorized(Req) ->
    case cowboy_req:headers(Req) of
        #{<<"authorization">> := <<"Bearer ", Token/binary>>} ->
            load_user_info(maps:put(oidcc_extract_authorization, Token, Req));
        #{<<"authorization">> := Authorization} ->
            send_invalid_header_response(Req, Authorization);
        #{} ->
            false
    end.

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
                    send_inactive_token_response(maps:put(?MODULE, undefined, Req));
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
            send_inactive_token_response(Req#{oidcc_introspect_token => Introspection});
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
                    send_inactive_token_response(Req#{oidcc_introspect_token => Introspection});
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
    case oidcc_client_context:from_configuration_worker(Provider, ClientId, ClientSecret) of
        {ok, ClientContext} ->
            case oidcc_token:validate_id_token(Token, ClientContext, any) of
                {ok, Claims} ->
                    {true, Req#{oidcc_validate_jwt_token => Claims}};
                {error, token_expired} ->
                    send_inactive_token_response(maps:put(?MODULE, undefined, Req));
                {error, Reason} ->
                    erlang:error(Reason)
            end;
        {error, token_expired} ->
            send_inactive_token_response(maps:put(?MODULE, undefined, Req));
        {error, Reason} ->
            erlang:error(Reason)
    end;
validate_jwt_token(#{}) ->
    erlang:error(no_oidcc_cowboy_extract_authorization).

send_inactive_token_response(Req0) ->
    Req = cowboy_req:reply(
        401,
        #{<<"content-type">> => <<"text/plain">>},
        <<"The provided token is inactive">>,
        Req0
    ),
    {stop, Req}.

send_invalid_header_response(Req0, GivenHeader) ->
    Req = cowboy_req:reply(
        400,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Invalid authorization Header\n\nExpected: Authorization: Bearer <token>\nGiven: ",
            GivenHeader/binary>>,
        Req0
    ),
    {stop, Req}.
