# Configuration

All configuration lives under the `nova_oidcc` application key in `sys.config`.

## Full Example

```erlang
{nova_oidcc, [
    {providers, #{
        google => #{
            issuer => <<"https://accounts.google.com">>,
            client_id => <<"123456.apps.googleusercontent.com">>,
            client_secret => <<"GOCSPX-...">>
        },
        github => #{
            issuer => <<"https://token.actions.githubusercontent.com">>,
            client_id => <<"Iv1.abc123">>,
            client_secret => <<"secret123">>
        }
    }},
    {base_url, <<"https://myapp.com">>},
    {scopes, [<<"openid">>, <<"profile">>, <<"email">>]},
    {on_success, {redirect, <<"/">>}},
    {on_failure, {redirect, <<"/login?error=auth_failed">>}}
]}
```

## Options

### `providers` (required)

A map of provider name atoms to provider configuration maps.

Each provider requires:

| Key | Type | Description |
|-----|------|-------------|
| `issuer` | `binary()` | The provider's OpenID Connect issuer URL |
| `client_id` | `binary()` | OAuth client ID from the provider |
| `client_secret` | `binary()` | OAuth client secret from the provider |

```erlang
{providers, #{
    google => #{
        issuer => <<"https://accounts.google.com">>,
        client_id => <<"...">>,
        client_secret => <<"...">>
    }
}}
```

### `base_url`

The base URL of your application. Used to construct redirect URIs.

- **Type:** `binary()`
- **Default:** `<<"http://localhost:8080">>`

The callback URL for a provider named `google` will be `{base_url}/auth/google/callback`.

### `scopes`

The OpenID Connect scopes to request from the provider.

- **Type:** `[binary()]`
- **Default:** `[<<"openid">>, <<"profile">>, <<"email">>]`

Common scopes:

| Scope | Description |
|-------|-------------|
| `openid` | Required for OIDC. Returns a sub (subject) claim |
| `profile` | Name, picture, locale, etc. |
| `email` | Email address and verification status |
| `phone` | Phone number |

### `on_success`

What to do after successful authentication.

- **Type:** `{redirect, binary()}`
- **Default:** `{redirect, <<"/">>}`

```erlang
{on_success, {redirect, <<"/dashboard">>}}
```

### `on_failure`

What to do when authentication fails.

- **Type:** `{redirect, binary()} | {status, integer()}`
- **Default:** `{status, 401}`

```erlang
%% Redirect to a login page with error
{on_failure, {redirect, <<"/login?error=auth_failed">>}}

%% Return HTTP 401
{on_failure, {status, 401}}
```

## Provider Setup

### Starting Providers

Each configured provider needs an `oidcc_provider_configuration_worker` running. Start them in your application's `start/2`:

```erlang
start(_Type, _Args) ->
    %% Start each provider
    maps:foreach(fun(Name, Config) ->
        {ok, _} = nova_oidcc:start_provider(Name, Config)
    end, application:get_env(nova_oidcc, providers, #{})),

    my_app_sup:start_link().
```

The worker fetches and caches the provider's OpenID configuration from their `.well-known/openid-configuration` endpoint.

### Environment Variables

For production, load secrets from environment variables:

```erlang
{nova_oidcc, [
    {providers, #{
        google => #{
            issuer => <<"https://accounts.google.com">>,
            client_id => {env, "GOOGLE_CLIENT_ID"},
            client_secret => {env, "GOOGLE_CLIENT_SECRET"}
        }
    }},
    {base_url, {env, "APP_BASE_URL"}}
]}
```

Then resolve them at startup:

```erlang
resolve_env({env, Var}) ->
    list_to_binary(os:getenv(Var));
resolve_env(Value) ->
    Value.
```

## Session Storage

Nova OIDCC stores authentication state in Nova's session system (ETS by default). The following session keys are used:

| Key | When | Contents |
|-----|------|----------|
| `oidcc_nonce` | During login flow | Cryptographic nonce for replay protection |
| `oidcc_pkce` | During login flow | PKCE code verifier |
| `oidcc_provider` | During login flow | Provider name |
| `oidcc_user` | After successful auth | Serialized userinfo claims |
| `oidcc_token` | After successful auth | Serialized token data |

The `oidcc_nonce`, `oidcc_pkce`, and `oidcc_provider` keys are cleaned up after callback processing.
