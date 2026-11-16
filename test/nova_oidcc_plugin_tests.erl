-module(nova_oidcc_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

-define(USER, #{~"sub" => ~"user-1", ~"email" => ~"user@example.com"}).
-define(REFUSE_WITH_401, #{on_unauthorized => {status, 401}}).

plugin_test_() ->
    {foreach, fun nova_oidcc_test_session:start/0, fun(_) -> nova_oidcc_test_session:stop() end, [
        fun pre_security_lets_a_signed_in_user_through/0,
        fun pre_security_answers_with_the_status/0,
        fun pre_security_answers_with_the_redirect/0,
        fun pre_request_passes_through_after_pre_security/0,
        fun pre_request_lets_a_signed_in_user_through/0,
        fun pre_request_answers_with_the_status/0,
        fun pre_request_answers_with_the_default_redirect/0,
        fun a_stored_user_that_is_not_a_binary_is_refused/0
    ]}.

pre_security_lets_a_signed_in_user_through() ->
    ok = sign_in(),
    {ok, Checked, plugin_state} =
        nova_oidcc_plugin:pre_security(request(), #{}, ?REFUSE_WITH_401, plugin_state),
    ?assertEqual(?USER, maps:get(oidcc_user, Checked)).

pre_security_answers_with_the_status() ->
    ?assertMatch(
        {stop, #{has_sent_resp := true}, plugin_state},
        nova_oidcc_plugin:pre_security(request(), #{}, ?REFUSE_WITH_401, plugin_state)
    ),
    ?assertMatch({401, _Headers}, sent_response()).

pre_security_answers_with_the_redirect() ->
    Options = #{on_unauthorized => {redirect, ~"/auth/keycloak/login"}},
    ?assertMatch(
        {stop, #{has_sent_resp := true}, plugin_state},
        nova_oidcc_plugin:pre_security(request(), #{}, Options, plugin_state)
    ),
    ?assertMatch({302, #{~"location" := ~"/auth/keycloak/login"}}, sent_response()).

pre_request_passes_through_after_pre_security() ->
    ok = sign_in(),
    {ok, Checked, plugin_state} =
        nova_oidcc_plugin:pre_security(request(), #{}, ?REFUSE_WITH_401, plugin_state),
    {ok, _SignedOut} = nova_session:delete(Checked),
    ?assertEqual(
        {ok, Checked, plugin_state},
        nova_oidcc_plugin:pre_request(Checked, #{}, ?REFUSE_WITH_401, plugin_state)
    ).

pre_request_lets_a_signed_in_user_through() ->
    ok = sign_in(),
    {ok, Checked, plugin_state} =
        nova_oidcc_plugin:pre_request(request(), #{}, ?REFUSE_WITH_401, plugin_state),
    ?assertEqual(?USER, maps:get(oidcc_user, Checked)).

pre_request_answers_with_the_status() ->
    ?assertMatch(
        {stop, #{has_sent_resp := true}, plugin_state},
        nova_oidcc_plugin:pre_request(request(), #{}, ?REFUSE_WITH_401, plugin_state)
    ),
    ?assertMatch({401, _Headers}, sent_response()).

pre_request_answers_with_the_default_redirect() ->
    ?assertMatch(
        {stop, #{has_sent_resp := true}, plugin_state},
        nova_oidcc_plugin:pre_request(request(), #{}, #{}, plugin_state)
    ),
    ?assertMatch({302, #{~"location" := ~"/auth/google/login"}}, sent_response()).

a_stored_user_that_is_not_a_binary_is_refused() ->
    ok = nova_oidcc_test_session:set_value(~"session-of-the-request", ~"oidcc_user", ?USER),
    ?assertMatch(
        {stop, #{has_sent_resp := true}, plugin_state},
        nova_oidcc_plugin:pre_request(request(), #{}, ?REFUSE_WITH_401, plugin_state)
    ),
    ?assertMatch({401, _Headers}, sent_response()).

sign_in() ->
    nova_session:set(request(), ~"oidcc_user", term_to_binary(?USER)).

request() ->
    #{
        pid => self(),
        streamid => 1,
        method => ~"GET",
        headers => #{},
        nova_session_id => ~"session-of-the-request"
    }.

sent_response() ->
    receive
        {{Pid, 1}, {response, Status, Headers, _Body}} when Pid =:= self() -> {Status, Headers}
    after 1000 -> no_response
    end.
