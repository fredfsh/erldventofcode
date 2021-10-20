-module(aoc_2016_19_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(1) ->
    1;
do(X) when X rem 2 =:= 0 ->
    do(X div 2) * 2 - 1;
do(X) ->
    case do((X + 1) div 2) of
        1 ->
            X;
        N ->
            N * 2 - 3
    end.
