-module(nova_oidcc_auth).

-export([is_authorized/1]).

is_authorized(#{headers := #{<<"authorization">> := <<"Bearer ", Token/binary>>}} = Req) ->
    case nova_oidcc:load_user_info(maps:put(oidcc_extract_authorization, Token, Req)) of
        {error, inactive_token} ->
            {false, 401, #{<<"content-type">> => <<"text/plain">>},
                <<"The provided token is inactive">>};
        {ok, #{oidcc_validate_jwt_token := Claims}} ->
            {true, Claims}
    end;
is_authorized(#{headers := #{<<"authorization">> := Authorization}}) ->
    {false, 400, #{<<"content-type">> => <<"text/plain">>},
        <<"Invalid authorization Header\n\nExpected: Authorization: Bearer <token>\nGiven: ",
            Authorization/binary>>};
is_authorized(_) ->
    {false, 400, #{<<"content-type">> => <<"text/plain">>},
        <<"Invalid authorization Header\n\nExpected: Authorization: Bearer <token>\n">>}.
