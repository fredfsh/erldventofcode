-module(aoc_2016_1_1).

-export([start/0]).

-define(SPACE_CHAR, 16#20).

start() ->
    Out = do(0, 0, {0, 1}),
    io:format("~p~n", [Out]),
    ok.

do(X, Y, Direction) ->
    case io:fread("", "~s") of
        eof ->
            abs(X) + abs(Y);
        {ok, [[Turn | T]]} ->
            NewDirection = turn(Turn, Direction),
            {Step, _} = string:to_integer(T),
            {NewX, NewY} = walk(X, Y, NewDirection, Step),
            do(NewX, NewY, NewDirection)
    end.

turn($R, {0, 1}) -> {1, 0};
turn($R, {1, 0}) -> {0, -1};
turn($R, {0, -1}) -> {-1, 0};
turn($R, {-1, 0}) -> {0, 1};
turn($L, {0, 1}) -> {-1, 0};
turn($L, {-1, 0}) -> {0, -1};
turn($L, {0, -1}) -> {1, 0};
turn($L, {1, 0}) -> {0, 1}.

walk(X, Y, {DX, DY}, Step) ->
    {X + DX * Step, Y + DY * Step}.
