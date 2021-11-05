-module(aoc_2017_19_1).

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

-define(SPACE, 16#20).

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            array:from_list(string:trim(L, trailing, "\n"), ?SPACE)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Arr) ->
    X = start(array:get(0, Arr)),
    trace([], X, 0, 0, 1, Arr).

start(Arr) ->
    start_impl(0, Arr).

start_impl(I, Arr) ->
    case array:get(I, Arr) of
        $| ->
            I;
        _ ->
            start_impl(I + 1, Arr)
    end.

trace(Acc, X, Y, DX, DY, Arr) ->
    NX = X + DX,
    NY = Y + DY,
    case array:get(NX, array:get(NY, Arr)) of
        ?SPACE ->
            lists:reverse(Acc);
        C when C >= $A, C =< $Z ->
            trace([C | Acc], NX, NY, DX, DY, Arr);
        C when C =:= $|; C =:= $- ->
            trace(Acc, NX, NY, DX, DY, Arr);
        $+ ->
            {NDX, NDY} = turn(NX, NY, DX, DY, Arr),
            trace(Acc, NX, NY, NDX, NDY, Arr)
    end.

turn(X, Y, DX, DY, Arr) ->
    {NDX, NDY} = turn_left(DX, DY),
    NX = X + NDX,
    NY = Y + NDY,
    case NX >= 0 andalso NY >= 0 andalso NY < array:size(Arr)
        andalso array:get(NX, array:get(NY, Arr)) =/= ?SPACE of

        true ->
            {NDX, NDY};
        false ->
            {-NDX, -NDY}
    end.

turn_left(1, 0) ->
    {0, -1};
turn_left(0, -1) ->
    {-1, 0};
turn_left(-1, 0) ->
    {0, 1};
turn_left(0, 1) ->
    {1, 0}.
