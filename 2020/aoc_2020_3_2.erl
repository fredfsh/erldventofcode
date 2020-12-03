-module(aoc_2020_3_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        [] ->
            Acc;
        Ls ->
            run_impl(Acc + do(Ls))
    end.

input() ->
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "~s") of
        eof ->
            lists:reverse(Acc);
        {ok, [L]} ->
            input_impl([L | Acc])
    end.

do(Ls) ->
    do(Ls, 1, 1) * do(Ls, 3, 1) * do(Ls, 5, 1) * do(Ls, 7, 1) * do(Ls, 1, 2).

do(Ls, Right, Down) ->
    NRow = length(Ls),
    NCol = length(lists:nth(1, Ls)),
    do_impl(Ls, NRow, NCol, Right, Down, 0, 1, 1).

do_impl(_Ls, NR, _NC, _R, _D, Acc, Row, _Col) when Row > NR ->
    Acc;
do_impl(Ls, NR, NC, R, D, Acc, Row, Col) when Col > NC ->
    do_impl(Ls, NR, NC, R, D, Acc, Row, Col - NC);
do_impl(Ls, NR, NC, R, D, Acc, Row, Col) ->
    Count = case lists:nth(Col, lists:nth(Row, Ls)) of
                $. -> 0;
                $# -> 1
            end,
    do_impl(Ls, NR, NC, R, D, Acc + Count, Row + D, Col + R).
