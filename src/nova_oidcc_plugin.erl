-module(nova_oidcc_plugin).
-moduledoc """
Nova plugin that protects routes by checking for an authenticated OIDC user.

## Usage

Add to route group plugins:

```erlang
#{
    prefix => "/protected",
    plugins => [
        {pre_request, nova_oidcc_plugin, #{
            on_unauthorized => {redirect, <<"/auth/google/login">>}
        }}
    ],
    routes => [...]
}
```

On a Nova that has the `pre_security/4` plugin callback the session is checked
there, ahead of the security callback of the route group. On an earlier Nova
it is checked in `pre_request/4`. A request without a user is answered by the
plugin and goes no further.

## Options

- `on_unauthorized` — action when no user session exists:
  - `{redirect, Url}` — redirect to login (default: `{redirect, <<"/auth/google/login">>}`)
  - `{status, Code}` — return HTTP status (e.g., `{status, 401}`)
- `session_key` — session key for user data (default: `<<"oidcc_user">>`)
""".

-behaviour(nova_plugin).

-export([pre_security/4, pre_request/4, post_request/4, plugin_info/0]).

-define(CHECKED, nova_oidcc_plugin_checked).

-spec pre_security(map(), term(), map(), term()) ->
    {ok, map(), term()} | {stop, map(), term()}.
pre_security(Req, _Env, Options, State) ->
    case require_user(Req, Options) of
        {ok, Req1} -> {ok, Req1#{?CHECKED => true}, State};
        {stop, Req1} -> {stop, Req1, State}
    end.

-spec pre_request(map(), term(), map(), term()) ->
    {ok, map(), term()} | {stop, map(), term()}.
pre_request(#{?CHECKED := true} = Req, _Env, _Options, State) ->
    {ok, Req, State};
pre_request(Req, _Env, Options, State) ->
    case require_user(Req, Options) of
        {ok, Req1} -> {ok, Req1, State};
        {stop, Req1} -> {stop, Req1, State}
    end.

-spec post_request(map(), term(), map(), term()) -> term().
post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

-spec plugin_info() -> map().
plugin_info() ->
    #{
        title => <<"Nova OIDC Plugin">>,
        version => <<"0.1.0">>,
        url => <<"https://github.com/novaframework/nova_oidcc">>,
        authors => [<<"Nova Team">>],
        description => <<"Protects routes by requiring OIDC authentication">>,
        options => [
            {on_unauthorized, <<"Action when unauthenticated: {redirect, Url} | {status, Code}">>},
            {session_key, <<"Session key for user data (default: oidcc_user)">>}
        ]
    }.

%%----------------------------------------------------------------------
%% Internal
%%----------------------------------------------------------------------

-spec require_user(map(), map()) -> {ok, map()} | {stop, map()}.
require_user(Req, Options) ->
    SessionKey = maps:get(session_key, Options, <<"oidcc_user">>),
    case nova_session:get(Req, SessionKey) of
        {ok, UserBin} when is_binary(UserBin) ->
            User = binary_to_term(UserBin),
            {ok, Req#{oidcc_user => User}};
        _ ->
            OnUnauth = maps:get(on_unauthorized, Options, {redirect, <<"/auth/google/login">>}),
            {stop, refuse(OnUnauth, Req)}
    end.

-spec refuse({redirect, iodata()} | {status, integer()}, map()) ->
    map().
refuse({redirect, Url}, Req) ->
    cowboy_req:reply(302, #{~"location" => Url}, Req);
refuse({status, Code}, Req) ->
    cowboy_req:reply(Code, Req).
