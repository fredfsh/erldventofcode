-module(aoc_2017_3_2).

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
    Values = maps:put({0, 0}, 1, maps:new()),
    do_impl({0, 0}, {1, 0}, Values, X).

do_impl({X, Y}, {DX, DY} = DXY, Values, Target) ->
    NXY = {X + DX, Y + DY},
    case value(NXY, Values) of
        N when N > Target ->
            N;
        N ->
            NDXY = maybe_turn_left(NXY, DXY, Values),
            do_impl(NXY, NDXY, maps:put(NXY, N, Values), Target)
    end.

-define(D, [{-1, -1}, { 0, -1}, { 1, -1},
            {-1,  0},           { 1,  0},
            {-1,  1}, { 0,  1}, { 1,  1}]).

value({X, Y}, Values) ->
    F = fun({DX, DY}, Acc) ->
                Acc + maps:get({X + DX, Y + DY}, Values, 0)
        end,
    lists:foldl(F, 0, ?D).

maybe_turn_left({X, Y}, DXY, Values) ->
    {NDX, NDY} = turn_left(DXY),
    case maps:is_key({X + NDX, Y + NDY}, Values) of
        true ->
            DXY;
        false ->
            {NDX, NDY}
    end.

turn_left({1, 0}) -> {0, -1};
turn_left({0, -1}) -> {-1, 0};
turn_left({-1, 0}) -> {0, 1};
turn_left({0, 1}) -> {1, 0}.
