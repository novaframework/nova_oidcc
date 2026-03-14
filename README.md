# Nova OIDCC

OpenID Connect integration for [Nova](https://github.com/novaframework/nova) using [erlef/oidcc](https://github.com/erlef/oidcc).

Add "Login with Google/GitHub/Keycloak/..." to your Nova application with minimal configuration. Built on the ERLEF Security Working Group's OpenID Certified client library.

## Features

- Multi-provider support (Google, GitHub, Microsoft, Keycloak, any OIDC provider)
- Authorization Code Flow with PKCE (secure by default)
- Automatic nonce generation and validation
- Userinfo retrieval
- Route protection via Nova plugin
- Session-based authentication state
- Configurable success/failure handling

## Installation

Add `nova_oidcc` to your `rebar.config` dependencies:

```erlang
{deps, [
    {nova_oidcc, {git, "https://github.com/novaframework/nova_oidcc.git", {branch, "main"}}}
]}.
```

## Quick Start

### 1. Configure a provider

In your `sys.config`:

```erlang
{nova_oidcc, [
    {providers, #{
        google => #{
            issuer => <<"https://accounts.google.com">>,
            client_id => <<"your-client-id">>,
            client_secret => <<"your-client-secret">>
        }
    }},
    {base_url, <<"http://localhost:8080">>},
    {scopes, [<<"openid">>, <<"profile">>, <<"email">>]},
    {on_success, {redirect, <<"/">>}},
    {on_failure, {status, 401}}
]}
```

### 2. Start the provider worker

In your application's `start/2`:

```erlang
start(_Type, _Args) ->
    {ok, ProviderConfig} = nova_oidcc:provider_config(google),
    {ok, _Pid} = nova_oidcc:start_provider(google, ProviderConfig),
    my_app_sup:start_link().
```

### 3. Add routes

In your router module:

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

### 4. Protect routes

Add the plugin to any route group that requires authentication:

```erlang
#{
    prefix => "/dashboard",
    plugins => [
        {pre_request, [{nova_oidcc_plugin, #{
            on_unauthorized => {redirect, <<"/auth/google/login">>}
        }}]}
    ],
    routes => [
        {"/", fun dashboard_controller:index/1, #{}}
    ]
}
```

### 5. Access user data in controllers

```erlang
index(Req = #{oidcc_user := User}) ->
    Email = maps:get(<<"email">>, User, <<"unknown">>),
    Name = maps:get(<<"name">>, User, <<"Anonymous">>),
    {json, #{email => Email, name => Name}}.
```

## Guides

- [Configuration](guides/configuration.md) — All configuration options
- [Multiple Providers](guides/multiple-providers.md) — Using Google, GitHub, Keycloak together
- [Route Protection](guides/route-protection.md) — Protecting routes and accessing user data

## How It Works

1. User visits `/auth/google/login`
2. Controller generates a cryptographic nonce and PKCE verifier, stores them in the Nova session
3. User is redirected to Google's authorization endpoint
4. User authenticates with Google
5. Google redirects back to `/auth/google/callback` with an authorization code
6. Controller exchanges the code for tokens, validates the nonce, and retrieves user info
7. User info is stored in the session
8. User is redirected to the configured success URL

## Requirements

- OTP 26+
- Nova 0.13+
- An OIDC provider with client credentials

## License

MIT
