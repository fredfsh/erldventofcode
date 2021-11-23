-module(aoc_2019_1_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(ini()).

run_impl(Acc) ->
    case input() of
        eof ->
            fin(Acc);
        X ->
            run_impl(acc(Acc, do(X)))
    end.

input() ->
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

do(X) ->
    case X div 3 - 2 of
        Y when Y < 0 ->
            0;
        Y ->
            Y + do(Y)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
