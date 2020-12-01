-module(aoc_2016_1_2).

-export([start/0]).

-define(SPACE_CHAR, 16#20).

start() ->
    Visited = sets:new(),
    Out = do(0, 0, {0, 1}, sets:add_element({0, 0}, Visited)),
    io:format("~p~n", [Out]),
    ok.

do(X, Y, Direction, Visited) ->
    {ok, [[Turn | T]]} = io:fread("", "~s"),
    NewDirection = turn(Turn, Direction),
    {Step, _} = string:to_integer(T),
    case walk(X, Y, NewDirection, Step, Visited) of
        {VisitedX, VisitedY} ->
            abs(VisitedX) + abs(VisitedY);
        {NewX, NewY, NewVisited} ->
            do(NewX, NewY, NewDirection, NewVisited)
    end.

turn($R, {0, 1}) -> {1, 0};
turn($R, {1, 0}) -> {0, -1};
turn($R, {0, -1}) -> {-1, 0};
turn($R, {-1, 0}) -> {0, 1};
turn($L, {0, 1}) -> {-1, 0};
turn($L, {-1, 0}) -> {0, -1};
turn($L, {0, -1}) -> {1, 0};
turn($L, {1, 0}) -> {0, 1}.

walk(X, Y, {_DX, _DY}, 0, Visited) ->
    {X, Y, Visited};
walk(X, Y, {DX, DY}, Step, Visited) ->
    NewX = X + DX,
    NewY = Y + DY,
    case sets:is_element({NewX, NewY}, Visited) of
        true ->
            {NewX, NewY};
        _ ->
            walk(NewX, NewY, {DX, DY}, Step - 1, sets:add_element({NewX, NewY}, Visited))
    end.
