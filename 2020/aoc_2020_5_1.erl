-module(aoc_2020_5_1).

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
            run_impl(max(Acc, do(X)))
    end.

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    row_impl(X, 64, 0).

row_impl(X, 0, Row) ->
    Row * 8 + col_impl(X, 4, 0);
row_impl([$F | T], Base, Row) ->
    row_impl(T, Base div 2, Row);
row_impl([$B | T], Base, Row) ->
    row_impl(T, Base div 2, Row + Base).

col_impl([], 0, Col) ->
    Col;
col_impl([$L | T], Base, Col) ->
    col_impl(T, Base div 2, Col);
col_impl([$R | T], Base, Col) ->
    col_impl(T, Base div 2, Col + Base).
