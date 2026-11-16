-module(nova_oidcc_controller_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SESSION_ID, ~"session-of-the-login").
-define(USER, #{~"sub" => ~"user-1", ~"email" => ~"user@example.com"}).
-define(TOKEN, {oidcc_token, id_token, access_token, refresh_token, [~"openid"]}).

signed_in_test_() ->
    {foreach, fun start/0, fun(_) -> nova_oidcc_test_session:stop() end, [
        fun user_and_token_are_stored_on_the_returned_request/0,
        fun no_login_state_is_left_behind/0,
        fun the_client_is_moved_to_a_new_session/0
    ]}.

start() ->
    ok = nova_oidcc_test_session:start(),
    ok = nova_session:set(login_request(), ~"oidcc_nonce", ~"the-nonce"),
    ok = nova_session:set(login_request(), ~"oidcc_pkce", ~"the-verifier"),
    ok = nova_session:set(login_request(), ~"oidcc_provider", ~"google").

user_and_token_are_stored_on_the_returned_request() ->
    {redirect, ~"/", SignedIn} = nova_oidcc_controller:signed_in(login_request(), ?TOKEN, ?USER),
    ?assertEqual(?USER, stored(SignedIn, ~"oidcc_user")),
    ?assertMatch(#{id := _, access := _, refresh := _}, stored(SignedIn, ~"oidcc_token")).

no_login_state_is_left_behind() ->
    {redirect, ~"/", SignedIn} = nova_oidcc_controller:signed_in(login_request(), ?TOKEN, ?USER),
    [
        ?assertEqual(
            {Request, Key, {error, not_found}}, {Request, Key, nova_session:get(Request, Key)}
        )
     || Request <- [login_request(), SignedIn],
        Key <- [~"oidcc_nonce", ~"oidcc_pkce", ~"oidcc_provider"]
    ].

the_client_is_moved_to_a_new_session() ->
    case nova_has_sign_in() of
        true ->
            {redirect, ~"/", SignedIn} =
                nova_oidcc_controller:signed_in(login_request(), ?TOKEN, ?USER),
            ?assertNotEqual(?SESSION_ID, maps:get(nova_session_id, SignedIn)),
            ?assertEqual({error, not_found}, nova_session:get(login_request(), ~"oidcc_user")),
            ?assertEqual({error, not_found}, nova_session:get(login_request(), ~"oidcc_token"));
        false ->
            ok
    end.

stored(Request, Key) ->
    case nova_session:get(Request, Key) of
        {ok, Stored} when is_binary(Stored) -> binary_to_term(Stored)
    end.

login_request() ->
    #{nova_session_id => ?SESSION_ID}.

nova_has_sign_in() ->
    _ = code:ensure_loaded(nova_session),
    erlang:function_exported(nova_session, sign_in, 3).
