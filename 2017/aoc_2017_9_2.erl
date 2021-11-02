-module(aoc_2017_9_2).

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
    do_impl(X, false, 0).

do_impl([], false, Acc) ->
    Acc;
do_impl([$< | T], false, Acc) ->
    do_impl(T, true, Acc);
do_impl([$> | T], true, Acc) ->
    do_impl(T, false, Acc);
do_impl([$!, _ | T], true, Acc) ->
    do_impl(T, true, Acc);
do_impl([_ | T], Garbage, Acc) ->
    do_impl(T, Garbage, Acc + if Garbage -> 1; true -> 0 end).


acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
