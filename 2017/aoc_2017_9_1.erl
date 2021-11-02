-module(aoc_2017_9_1).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

do(X) ->
    do_impl(X, 0, false, 0).

do_impl([], _, false, Acc) ->
    Acc;
do_impl([${ | T], Level, false, Acc) ->
    do_impl(T, Level + 1, false, Acc);
do_impl([$} | T], Level, false, Acc) ->
    do_impl(T, Level - 1, false, Acc + Level);
do_impl([$< | T], Level, false, Acc) ->
    do_impl(T, Level, true, Acc);
do_impl([$> | T], Level, true, Acc) ->
    do_impl(T, Level, false, Acc);
do_impl([$!, _ | T], Level, true, Acc) ->
    do_impl(T, Level, true, Acc);
do_impl([_ | T], Level, Garbage, Acc) ->
    do_impl(T, Level, Garbage, Acc).


acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
