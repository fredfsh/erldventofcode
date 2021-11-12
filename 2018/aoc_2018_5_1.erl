-module(aoc_2018_5_1).

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
    do_impl([], X).

do_impl(Acc, []) ->
    length(Acc);
do_impl([], [H | T]) ->
    do_impl([H], T);
do_impl([HH | TT], [H | T]) when abs(H - HH) =:= abs($A - $a) ->
    do_impl(TT, T);
do_impl(L, [H | T]) ->
    do_impl([H | L], T).


acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
