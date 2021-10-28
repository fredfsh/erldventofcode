-module(aoc_2017_3_1).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    Visited = sets:add_element({0, 0}, sets:new()),
    do_impl(1, {0, 0}, {1, 0}, Visited, X).

do_impl(N, {X, Y}, {DX, DY} = DXY, Visited, Target) ->
    NXY = {NX, NY} = {X + DX, Y + DY},
    case N + 1 of
        Target ->
            abs(NX) + abs(NY);
        _ ->
            NDXY = maybe_turn_left(NXY, DXY, Visited),
            do_impl(N + 1, NXY, NDXY, sets:add_element(NXY, Visited), Target)
    end.

maybe_turn_left({X, Y}, DXY, Visited) ->
    {NDX, NDY} = turn_left(DXY),
    case sets:is_element({X + NDX, Y + NDY}, Visited) of
        true ->
            DXY;
        false ->
            {NDX, NDY}
    end.

turn_left({1, 0}) -> {0, -1};
turn_left({0, -1}) -> {-1, 0};
turn_left({-1, 0}) -> {0, 1};
turn_left({0, 1}) -> {1, 0}.
