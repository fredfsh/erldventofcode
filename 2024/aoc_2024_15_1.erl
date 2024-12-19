-module(aoc_2024_15_1).

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

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [[$# | _] = L]} ->
            array:from_list(L);
        {ok, [L]} ->
            L
    end.

ini() ->
    {array:new(), []}.

do(X) ->
    X.

acc({Arr, L}, X) when is_list(X) ->
    {Arr, L ++ X};
acc({Arr, L}, X) ->
    {array:set(array:size(Arr), X, Arr), L}.

fin({Arr, Moves}) ->
%%    io:format("~p~n~p~n", [array:to_list(Arr), Moves]),
    {Robot, Boxes, Walls} = graph(Arr),
%%    io:format("~p~n~p~n~p~n", [Robot, Boxes, Walls]),
    F = fun(Move, {RobotAcc, BoxesAcc}) ->
                move(RobotAcc, BoxesAcc, Move, Walls)
        end,
    {_, NewBoxes} = lists:foldl(F, {Robot, Boxes}, Moves),
    gps(NewBoxes).

graph(Arr2) ->
%%    io:format("~p~n~p~n", [Arr2, array:to_list(Arr2)]),
    F = fun(Y, Arr, FAcc) ->
%%                io:format("~p: ~p~n", [Y, Arr]),
                G = fun(_, $., GAcc) ->
                            GAcc;
                       (X, $@, {_, BoxesAcc, WallsAcc}) ->
                            {{X, Y}, BoxesAcc, WallsAcc};
                       (X, $O, {RobotAcc, BoxesAcc, WallsAcc}) ->
                            Boxes = sets:add_element({X, Y}, BoxesAcc),
                            {RobotAcc, Boxes, WallsAcc};
                       (X, $#, {RobotAcc, BoxesAcc, WallsAcc}) ->
                            Walls = sets:add_element({X, Y}, WallsAcc),
                            {RobotAcc, BoxesAcc, Walls}
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, {undefined, sets:new(), sets:new()}, Arr2).

-define(BOX(X, Y), sets:is_element({X, Y}, Boxes)).
-define(WALL(X, Y), sets:is_element({X, Y}, Walls)).

move({RX, RY} = RXY, Boxes, Move, Walls) ->
    {DX, DY} = d(Move),
    {NX, NY} = NXY = {RX + DX, RY + DY},
    case {?WALL(NX, NY), ?BOX(NX, NY)} of
        {false, false} ->
            {NXY, Boxes};
        {true, false} ->
            {RXY, Boxes};
        {false, true} ->
            {NBX, NBY} = NBXY = nonbox(NX, NY, DX, DY, Boxes),
            case ?WALL(NBX, NBY) of
                true ->
                    {RXY, Boxes};
                false ->
                    NBoxes = sets:del_element(NXY, Boxes),
                    {NXY, sets:add_element(NBXY, NBoxes)}
            end
    end.

d($^) -> { 0, -1};
d($v) -> { 0,  1};
d($<) -> {-1,  0};
d($>) -> { 1,  0}.

nonbox(X, Y, DX, DY, Boxes) ->
    case ?BOX(X, Y) of
        true ->
            nonbox(X + DX, Y + DY, DX, DY, Boxes);
        false ->
            {X, Y}
    end.

gps(Boxes) ->
    F = fun({X, Y}, Acc) ->
                Acc + 100 * Y + X
        end,
    sets:fold(F, 0, Boxes).
