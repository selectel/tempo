%%% @doc This module provides some code to make basic benchmarks.
%%%      One can see some results in benches folder, where results
%%%      are saved with appropriate date and short commit hash.
%%% @end
-module(ecirca_bench).
-export([start/0, avg_error/0]).
-define(WITH_N(F), fun(N) -> fun() -> F end end).

%% @private
max_value(small) -> 4095;
max_value(medium) -> 134217727;
max_value(large) -> 576460752303423487.

%% @doc Starts a benchmark.
%% @end
start() ->
    [bench_fun(Fun, Type) || Fun <- [push, set, update],
                             Type <- [last, min, avg]],
    ok.

%% @private
bench_fun(Fun, Type) ->
    io:format("~n======= ~s ~s ======================~n", [Fun, Type]),
    [bench_1_1k_max(Fun, Type, ValueSize)
     || ValueSize <- [small, medium, large]],
    ok.

%% @private
bench_1_1k_max(Fun, Type, ValueSize) ->
    io:format("------- ~s ----------------------~n", [ValueSize]),
    {ok, E} = ecirca:new(100, Type, ValueSize),
    [bench(io_lib:format("~s ~p", [Fun, X]),
           ?WITH_N(doN(Fun, E, 1, N)))
     || X <- [1, 1000, max_value(ValueSize)]],
    ok.

%% @private
bench(Name, Fun) ->
    bench(Name, Fun, [1000000]).

%% @private
bench(Name, Fun, Ns) when is_list(Ns) ->
    [bench(Name, Fun, N) || N <- Ns];
bench(Name, Fun, N) when is_integer(N) ->
    {Time, _} = timer:tc(Fun(N)),
    io:format("~s runned ~p times, took ~p us (~p us each)~n",
              [Name, N, Time, Time / N]).

%% @private
%% Quite ugly code, in fact.
doN(_, _, _, 0) -> ok;
doN(push, Ecirca, Val, N) ->
    ecirca:push(Ecirca, Val), doN(push, Ecirca, Val, N-1);
doN(set, Ecirca, Val, N) ->
    ecirca:set(Ecirca, 13, Val), doN(set, Ecirca, Val, N-1);
doN(update, Ecirca, Val, N) ->
    ecirca:update(Ecirca, 13, Val), doN(update, Ecirca, Val, N-1).

%% @doc Function that checks if avg ecirca is correct - prints a table of
%%      true averages and averages that was returned by ecirca.
%% @end
avg_error() ->
    {ok, Ec} = ecirca:new(3, avg, large),
    ecirca:update(Ec, 1, 1),
    io:format("Ecirca\tTrue\tDiff~n"),
    avg_error(Ec, [1], 2, 50).

%% @private
avg_error(_Ec, _Lst, N, Max) when N > Max-> ok;
avg_error(Ec, Lst, N, Max) ->
    {ok, EcAvg} = ecirca:get(Ec, 1),
    TrueAvg = avg(Lst),
    io:format("~p\t~p\t~p~n", [EcAvg, TrueAvg, abs(TrueAvg - EcAvg)]),
    ecirca:update(Ec, 1, N),
    avg_error(Ec, [N|Lst], N+1, Max).

%% @private
avg(Lst) -> sum(Lst) / length(Lst).

%% @private
sum(Lst) -> lists:foldl(fun (A, B) -> A + B end, 0, Lst).
