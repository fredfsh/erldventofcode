-module(aoc_2016_19_2).

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
do(N) ->
    case do(N - 1) of
        X when X =:= N - 1 ->
            1;
        X when X < N div 2 ->
            X + 1;
        X ->
            X + 2
    end.
