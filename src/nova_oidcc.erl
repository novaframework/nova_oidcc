-module(nova_oidcc).
-moduledoc """
OpenID Connect integration for Nova.

## Configuration

```erlang
%% sys.config
{nova_oidcc, [
    {providers, #{
        google => #{
            issuer => <<"https://accounts.google.com">>,
            client_id => <<"your-client-id">>,
            client_secret => <<"your-client-secret">>
        }
    }},
    {scopes, [<<"openid">>, <<"profile">>, <<"email">>]},
    {on_success, {redirect, <<"/">>}},
    {on_failure, {status, 401}}
]}
```

## Routes

Add to your router:

```erlang
routes(_Env) ->
    [#{
        prefix => "/auth",
        security => false,
        routes => [
            {"/:provider/login", fun nova_oidcc_controller:login/1, #{}},
            {"/:provider/callback", fun nova_oidcc_controller:callback/1, #{}}
        ]
    }].
```

## Plugin

Protect routes with the authentication plugin:

```erlang
{pre_request, [{nova_oidcc_plugin, #{
    on_unauthorized => {redirect, <<"/auth/google/login">>}
}}]}
```
""".

-export([
    config/0,
    provider_config/1,
    provider_name/1,
    redirect_uri/1,
    start_provider/2
]).

-doc "Return the full nova_oidcc application configuration.".
-spec config() -> map().
config() ->
    Providers = application:get_env(nova_oidcc, providers, #{}),
    Scopes = application:get_env(nova_oidcc, scopes, [<<"openid">>, <<"profile">>, <<"email">>]),
    OnSuccess = application:get_env(nova_oidcc, on_success, {redirect, <<"/">>}),
    OnFailure = application:get_env(nova_oidcc, on_failure, {status, 401}),
    BaseUrl = application:get_env(nova_oidcc, base_url, <<"http://localhost:8080">>),
    #{
        providers => Providers,
        scopes => Scopes,
        on_success => OnSuccess,
        on_failure => OnFailure,
        base_url => BaseUrl
    }.

-doc "Return configuration for a specific provider.".
-spec provider_config(atom()) -> {ok, map()} | {error, not_found}.
provider_config(Provider) ->
    Providers = application:get_env(nova_oidcc, providers, #{}),
    case maps:find(Provider, Providers) of
        {ok, Config} -> {ok, Config};
        error -> {error, not_found}
    end.

-doc "Build the redirect URI for a provider.".
-spec redirect_uri(atom()) -> binary().
redirect_uri(Provider) ->
    BaseUrl = application:get_env(nova_oidcc, base_url, <<"http://localhost:8080">>),
    ProviderBin = atom_to_binary(Provider, utf8),
    <<BaseUrl/binary, "/auth/", ProviderBin/binary, "/callback">>.

-doc """
Start an oidcc provider configuration worker.

Call this from your application's `start/2` or supervision tree.
Returns the worker pid.
""".
-spec start_provider(atom(), map()) -> {ok, pid()} | {error, term()}.
start_provider(Name, #{issuer := Issuer}) ->
    oidcc_provider_configuration_worker:start_link(#{
        issuer => Issuer,
        name => {local, provider_name(Name)}
    }).

provider_name(Provider) ->
    list_to_atom("nova_oidcc_" ++ atom_to_list(Provider)).
