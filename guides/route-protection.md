# Route Protection

Nova OIDCC provides a plugin that protects routes by requiring an authenticated OIDC session.

## Basic Usage

Add `nova_oidcc_plugin` to any route group's `pre_request` plugins:

```erlang
routes(_Env) ->
    [
        %% Public routes (no auth required)
        #{
            prefix => "/",
            security => false,
            routes => [
                {"/", fun page_controller:index/1, #{}},
                {"/about", fun page_controller:about/1, #{}}
            ]
        },
        %% Auth routes (must be public)
        #{
            prefix => "/auth",
            security => false,
            routes => [
                {"/:provider/login", fun nova_oidcc_controller:login/1, #{}},
                {"/:provider/callback", fun nova_oidcc_controller:callback/1, #{}}
            ]
        },
        %% Protected routes
        #{
            prefix => "/dashboard",
            plugins => [
                {pre_request, [{nova_oidcc_plugin, #{
                    on_unauthorized => {redirect, <<"/auth/google/login">>}
                }}]}
            ],
            routes => [
                {"/", fun dashboard_controller:index/1, #{}},
                {"/settings", fun dashboard_controller:settings/1, #{}}
            ]
        }
    ].
```

## Plugin Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `on_unauthorized` | `{redirect, binary()} \| {status, integer()}` | `{redirect, <<"/auth/google/login">>}` | Action when user is not authenticated |
| `session_key` | `binary()` | `<<"oidcc_user">>` | Session key to check for user data |

### Redirect on Unauthorized

The default behavior redirects unauthenticated users to the login page:

```erlang
{nova_oidcc_plugin, #{
    on_unauthorized => {redirect, <<"/auth/google/login">>}
}}
```

### API-Style 401 Response

For JSON APIs, return a 401 status instead of redirecting:

```erlang
{nova_oidcc_plugin, #{
    on_unauthorized => {status, 401}
}}
```

## Accessing User Data

When the plugin authenticates a request, it adds `oidcc_user` to the Req map. Access it in your controllers via pattern matching:

```erlang
index(#{oidcc_user := User} = _Req) ->
    Email = maps:get(<<"email">>, User, <<"unknown">>),
    Name = maps:get(<<"name">>, User, <<"Anonymous">>),
    Picture = maps:get(<<"picture">>, User, <<>>),
    {json, #{
        email => Email,
        name => Name,
        picture => Picture
    }}.
```

### Common Userinfo Claims

The claims available depend on the provider and requested scopes:

| Claim | Type | Scope Required | Description |
|-------|------|----------------|-------------|
| `sub` | `binary()` | `openid` | Unique user identifier |
| `name` | `binary()` | `profile` | Full name |
| `given_name` | `binary()` | `profile` | First name |
| `family_name` | `binary()` | `profile` | Last name |
| `picture` | `binary()` | `profile` | Profile picture URL |
| `email` | `binary()` | `email` | Email address |
| `email_verified` | `boolean()` | `email` | Whether email is verified |
| `locale` | `binary()` | `profile` | Language/locale |

## Mixing Protected and Public Routes

You can have different protection strategies per route group:

```erlang
routes(_Env) ->
    [
        %% Public API
        #{
            prefix => "/api/v1/public",
            routes => [
                {"/status", fun api_controller:status/1, #{}}
            ]
        },
        %% Protected API (returns 401)
        #{
            prefix => "/api/v1",
            plugins => [
                {pre_request, [{nova_oidcc_plugin, #{
                    on_unauthorized => {status, 401}
                }}]}
            ],
            routes => [
                {"/me", fun api_controller:me/1, #{}},
                {"/settings", fun api_controller:settings/1, #{}}
            ]
        },
        %% Protected web pages (redirects to login)
        #{
            prefix => "/app",
            plugins => [
                {pre_request, [{nova_oidcc_plugin, #{
                    on_unauthorized => {redirect, <<"/auth/google/login">>}
                }}]}
            ],
            routes => [
                {"/", fun app_controller:index/1, #{}}
            ]
        }
    ].
```

## Using Nova's Security Callback Instead

As an alternative to the plugin, you can use Nova's built-in security callback mechanism:

```erlang
routes(_Env) ->
    [#{
        prefix => "/dashboard",
        security => fun auth:check/1,
        routes => [...]
    }].
```

```erlang
-module(auth).
-export([check/1]).

check(Req) ->
    case nova_session:get(Req, <<"oidcc_user">>) of
        {ok, UserBin} ->
            User = binary_to_term(UserBin),
            {true, User};
        {error, _} ->
            {redirect, <<"/auth/google/login">>}
    end.
```

The authenticated data is then available as `Req#{auth_data := User}` in controllers.
