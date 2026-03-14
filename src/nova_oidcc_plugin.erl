-module(nova_oidcc_plugin).
-moduledoc """
Nova plugin that protects routes by checking for an authenticated OIDC user.

## Usage

Add to route group plugins:

```erlang
#{
    prefix => "/protected",
    plugins => [
        {pre_request, [{nova_oidcc_plugin, #{
            on_unauthorized => {redirect, <<"/auth/google/login">>}
        }}]}
    ],
    routes => [...]
}
```

## Options

- `on_unauthorized` — action when no user session exists:
  - `{redirect, Url}` — redirect to login (default: `{redirect, <<"/auth/google/login">>}`)
  - `{status, Code}` — return HTTP status (e.g., `{status, 401}`)
- `session_key` — session key for user data (default: `<<"oidcc_user">>`)
""".

-behaviour(nova_plugin).

-export([pre_request/4, post_request/4, plugin_info/0]).

-spec pre_request(map(), term(), map(), term()) -> term().
pre_request(Req, _Env, Options, State) ->
    SessionKey = maps:get(session_key, Options, <<"oidcc_user">>),
    case nova_session:get(Req, SessionKey) of
        {ok, UserBin} ->
            User = binary_to_term(UserBin),
            {ok, Req#{oidcc_user => User}, State};
        {error, _} ->
            OnUnauth = maps:get(on_unauthorized, Options, {redirect, <<"/auth/google/login">>}),
            case OnUnauth of
                {redirect, Url} ->
                    {break, {redirect, Url}, Req, State};
                {status, Code} ->
                    {break, {status, Code}, Req, State}
            end
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
