-module(aoc_2016_18_1).

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
    aggregate(0, 0, X).

aggregate(Safes, Rows, L) ->
    next(Safes + count(L), Rows + 1, L).

count(L) ->
    lists:sum([1 || $. <- L]).

-define(ROWS, 40).

next(Safes, ?ROWS, _) ->
    Safes;
next(Safes, Rows, L) ->
    Row = row(L),
    aggregate(Safes, Rows, Row).

-define(SAFE, $.).
-define(TRAP, $^).

row(L) ->
    L2 = lists:append([[?SAFE], L, [?SAFE]]),
    row_impl(L2, []).

row_impl([_, _], Acc) ->
    lists:reverse(Acc);
row_impl([A, B, C | T], Acc) ->
    row_impl([B, C | T], [trap(A, B, C) | Acc]).

trap(?TRAP, ?TRAP, ?SAFE) -> ?TRAP;
trap(?SAFE, ?TRAP, ?TRAP) -> ?TRAP;
trap(?TRAP, ?SAFE, ?SAFE) -> ?TRAP;
trap(?SAFE, ?SAFE, ?TRAP) -> ?TRAP;
trap(_, _, _) -> ?SAFE.
