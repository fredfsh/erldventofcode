-module(aoc_2017_17_2).

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
    do_impl(0, undefined, 0, 1, X).

-define(N, 50000000).

do_impl(?N, After, _Cur, _Len, _) ->
    After;
do_impl(I, After, Cur, Len, Steps) ->
    case (Cur + Steps) rem Len of
        0 ->
            do_impl(I + 1, I + 1, 1, Len + 1, Steps);
        X ->
            do_impl(I + 1, After, X + 1, Len + 1, Steps)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
