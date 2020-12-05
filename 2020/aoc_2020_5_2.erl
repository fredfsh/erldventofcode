-module(aoc_2020_5_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Set = run_impl(sets:new()),
    find(1, Set).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(sets:add_element(do(X), Acc))
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

find(I, Set) ->
    case {sets:is_element(I + 1, Set),
          sets:is_element(I - 1, Set),
          sets:is_element(I, Set)} of
        {true, true, false} ->
            I;
        _ ->
            find(I + 1, Set)
    end.
