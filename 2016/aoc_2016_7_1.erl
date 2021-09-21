-module(aoc_2016_7_1).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    do_impl(X, false, $#, $#, $#, false).

do_impl([], _, _, _, _, true) ->
    1;
do_impl([], _, _, _, _, false) ->
    0;
do_impl([$[ | T], false, _, _, _, Found) ->
    do_impl(T, true, $#, $#, $#, Found);
do_impl([$] | T], true, _, _, _, Found) ->
    do_impl(T, false, $#, $#, $#, Found);
do_impl([A | _], true, B, B, A, _) when B =/= A ->
    0;
do_impl([A | T], false, B, B, A, _) when B =/= A ->
    do_impl(T, false, A, B, B, true);
do_impl([X | T], Forbid, P1, P2, _P3, Found) ->
    do_impl(T, Forbid, X, P1, P2, Found).
