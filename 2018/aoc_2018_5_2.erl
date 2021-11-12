-module(aoc_2018_5_2).

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
    L = [do_impl([], X, C) || C <- lists:seq($A, $Z)],
    hd(lists:sort(L)).

do_impl(Acc, [], _) ->
    length(Acc);
do_impl(Acc, [H | T], C) when H =:= C; H =:= C + ($a - $A)->
    do_impl(Acc, T, C);
do_impl([], [H | T], C) ->
    do_impl([H], T, C);
do_impl([HH | TT], [H | T], C) when abs(H - HH) =:= abs($A - $a) ->
    do_impl(TT, T, C);
do_impl(L, [H | T], C) ->
    do_impl([H | L], T, C).


acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
