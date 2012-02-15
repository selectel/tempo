-module(ecirca_tests).
-include_lib("eunit/include/eunit.hrl").

-define(LARGE_VALUE, math:pow(2, 50)).
-define(BIG_VALUE, math:pow(10, 6)).

new_test_() ->
    [?_assertMatch({ok, _}, ecirca:new(3, last)),
     ?_assertError(badarg, ecirca:new(-1, last)),
     ?_assertError(badarg, ecirca:new(0, last)),
     ?_assertError(badarg, ecirca:new(?LARGE_VALUE, last))].

%% -------------------------------------------------------------------

push_test_() ->
    {setup, local,
     fun () -> {ok, C} = ecirca:new(5, last), C end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun push_subtest_badarg/1,
                  fun push_subtest_val/1]}
     end}.

push_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:push(foobar, 1)),
    ?assertError(badarg, ecirca:push(C, -1)),
    ?assertError(badarg, ecirca:push(C, ?LARGE_VALUE)).

push_subtest_val(C) ->
    [?assertEqual(ok, ecirca:push(C, X)) || X <- lists:seq(1, 10)].

%% -------------------------------------------------------------------

get_test_() ->
    {setup, local,
     fun () -> {ok, C} = ecirca:new(5, last), C end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun get_subtest_badarg/1,
                  fun get_subtest_found/1,
                  fun get_subtest_val1/1,
                  fun get_subtest_val2/1]}
     end}.

get_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:get(foobar, 1)),
    ?assertError(badarg, ecirca:get(C, -1)),
    ?assertError(badarg, ecirca:get(C, 0)),
    ?assertError(badarg, ecirca:get(C, ?LARGE_VALUE)),
    ?assertError(badarg, ecirca:get(C, 6)).

get_subtest_found(C) ->
    ?assertEqual({ok, empty}, ecirca:get(C, 1)),
    ?assertEqual(ok, ecirca:push(C, 13)),
    ?assertEqual({ok, 13}, ecirca:get(C, 1)),
    [?assertEqual({ok, empty}, ecirca:get(C, X))
     || X <- lists:seq(2, 5)].

get_subtest_val1(C) ->
    [?assertEqual(ok, ecirca:push(C, X)) || X <- lists:seq(1, 5)],
     [?assertEqual({ok, 5 - X + 1}, ecirca:get(C, X))
     || X <- lists:seq(1, 5)].

get_subtest_val2(C) ->
    [?assertEqual(ok, ecirca:push(C, X)) || X <- lists:seq(1, 10)],
    [?assertEqual({ok, 10 - X + 1}, ecirca:get(C, X))
     || X <- lists:seq(1, 5)].

%% -------------------------------------------------------------------

set_test_() ->
    {setup, local,
     fun () -> {ok, C} = ecirca:new(5, last), C end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun set_subtest_badarg/1,
                  fun set_subtest_nfound/1,
                  fun get_subtest_val/1]}
     end}.

set_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:set(foobar, 1, 13)),
    ?assertError(badarg, ecirca:set(C, -1, 13)),
    ?assertError(badarg, ecirca:set(C, 0, 13)),
    ?assertError(badarg, ecirca:set(C, ?LARGE_VALUE, 13)),
    ?assertError(badarg, ecirca:set(C, 6, 13)),
    ?assertError(badarg, ecirca:set(C, 1, ?LARGE_VALUE)),
    ?assertError(badarg, ecirca:set(C, 1, -1)).
set_subtest_nfound(C) ->
    ?assertEqual({ok, {empty, 1}}, ecirca:set(C, 1, 1)),
    ?assertEqual(ok, ecirca:push(C, 13)),
    ?assertEqual({ok, {13, 14}}, ecirca:set(C, 1, 14)),
    [?assertEqual({ok, empty}, ecirca:get(C, X))
     || X <- lists:seq(3, 5)].

get_subtest_val(C) ->
    [?assertEqual(ok, ecirca:push(C, X)) || X <- lists:seq(1, 5)],
     [?assertEqual({ok, 5 - X + 1}, ecirca:get(C, X))
     || X <- lists:seq(1, 5)].

%% -------------------------------------------------------------------

save_test() ->
    Res = {ok, [3, 2, 1, empty, empty]},
    {ok, C} = ecirca:new(5, last),
    ecirca:push(C, 1),
    ecirca:push(C, 2),
    ecirca:push(C, 3),
    ?assertEqual(ecirca:slice(C, 1, 5), Res),
    {ok, B} = ecirca:save(C),
    {ok, NewC} = ecirca:load(B),
    ?assertEqual(ecirca:slice(NewC, 1, 5), Res).

%% -------------------------------------------------------------------

