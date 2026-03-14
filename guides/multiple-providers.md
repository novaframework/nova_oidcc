# Multiple Providers

Nova OIDCC supports multiple identity providers simultaneously. Users can choose which provider to authenticate with.

## Configuration

Define all providers in `sys.config`:

```erlang
{nova_oidcc, [
    {providers, #{
        google => #{
            issuer => <<"https://accounts.google.com">>,
            client_id => <<"google-client-id">>,
            client_secret => <<"google-client-secret">>
        },
        github => #{
            issuer => <<"https://token.actions.githubusercontent.com">>,
            client_id => <<"github-client-id">>,
            client_secret => <<"github-client-secret">>
        },
        keycloak => #{
            issuer => <<"https://keycloak.mycompany.com/realms/myapp">>,
            client_id => <<"nova-app">>,
            client_secret => <<"keycloak-secret">>
        }
    }},
    {base_url, <<"https://myapp.com">>}
]}
```

## Starting All Providers

Start all configured providers at application boot:

```erlang
start(_Type, _Args) ->
    Providers = application:get_env(nova_oidcc, providers, #{}),
    maps:foreach(fun(Name, Config) ->
        {ok, _} = nova_oidcc:start_provider(Name, Config)
    end, Providers),
    my_app_sup:start_link().
```

## Routes

The same two routes handle all providers via the `:provider` path parameter:

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

This creates endpoints for each provider:

| Provider | Login URL | Callback URL |
|----------|-----------|--------------|
| Google | `/auth/google/login` | `/auth/google/callback` |
| GitHub | `/auth/github/login` | `/auth/github/callback` |
| Keycloak | `/auth/keycloak/login` | `/auth/keycloak/callback` |

## Login Page

Create a login page that links to each provider:

```erlang
login(Req) ->
    {ok, [{providers, [
        #{name => <<"Google">>, url => <<"/auth/google/login">>},
        #{name => <<"GitHub">>, url => <<"/auth/github/login">>},
        #{name => <<"Keycloak">>, url => <<"/auth/keycloak/login">>}
    ]}]}.
```

Template:

```html
<h1>Sign In</h1>
{% for provider in providers %}
  <a href="{{ provider.url }}">Sign in with {{ provider.name }}</a>
{% endfor %}
```

## Provider-Specific Scopes

If different providers need different scopes, override per-provider in the config:

```erlang
{providers, #{
    google => #{
        issuer => <<"https://accounts.google.com">>,
        client_id => <<"...">>,
        client_secret => <<"...">>,
        scopes => [<<"openid">>, <<"profile">>, <<"email">>, <<"calendar.readonly">>]
    },
    github => #{
        issuer => <<"https://token.actions.githubusercontent.com">>,
        client_id => <<"...">>,
        client_secret => <<"...">>,
        scopes => [<<"openid">>, <<"read:user">>, <<"user:email">>]
    }
}}
```

## Identifying the Provider After Login

After authentication, the userinfo claims are stored in the session. The `iss` (issuer) claim identifies which provider authenticated the user:

```erlang
index(Req = #{oidcc_user := User}) ->
    Issuer = maps:get(<<"iss">>, User, <<"unknown">>),
    Provider = case Issuer of
        <<"https://accounts.google.com">> -> google;
        <<"https://token.actions.githubusercontent.com">> -> github;
        _ -> unknown
    end,
    {json, #{provider => Provider, user => User}}.
```

## Registering Callback URLs with Providers

Each provider requires you to register the callback URL in their developer console:

| Provider | Console | Callback URL |
|----------|---------|--------------|
| Google | [Google Cloud Console](https://console.cloud.google.com/apis/credentials) | `https://myapp.com/auth/google/callback` |
| GitHub | [GitHub Developer Settings](https://github.com/settings/developers) | `https://myapp.com/auth/github/callback` |
| Keycloak | Keycloak Admin Console → Clients → your client | `https://myapp.com/auth/keycloak/callback` |
