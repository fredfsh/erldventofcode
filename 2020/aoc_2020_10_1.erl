-module(aoc_2020_10_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        [] ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl([]).

input_impl(L) ->
    case io:fread("", "~d") of
        eof ->
            L;
        {ok, [X]} ->
            input_impl([X | L])
    end.

do(L) ->
    L2 = lists:sort(L),
    do_impl(L2, 0, 0, 1).

do_impl([], _Last, Ones, Threes) ->
    Ones * Threes;
do_impl([X | T], Last, Ones, Threes) when X =:= Last + 1 ->
    do_impl(T, X, Ones + 1, Threes);
do_impl([X | T], Last, Ones, Threes) when X =:= Last + 3 ->
    do_impl(T, X, Ones, Threes + 1);
do_impl([X | T], _Last, Ones, Threes) ->
    do_impl(T, X, Ones, Threes).
