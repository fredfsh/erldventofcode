-module(aoc_2024_16_1).

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
        {ok, [X]} ->
            array:from_list(X)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(EAST,  { 1,  0}).
-define(NORTH, { 0, -1}).
-define(SOUTH, { 0,  1}).
-define(WEST,  {-1,  0}).

fin(Graph) ->
    {Start, End, Walls} = graph(Graph),
    Init = {Start, ?EAST},
    astar(0, maps:from_list([{0, [Init]}]), sets:from_list([Init]), End, Walls).

graph(Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $S, {_, EndAcc, WallsAcc}) ->
                            {{X, Y}, EndAcc, WallsAcc};
                       (X, $E, {StartAcc, _, WallsAcc}) ->
                            {StartAcc, {X, Y}, WallsAcc};
                       (X, $#, {StartAcc, EndAcc, WallsAcc}) ->
                            Walls = sets:add_element({X, Y}, WallsAcc),
                            {StartAcc, EndAcc, Walls};
                       (_, $., GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, {undefined, undefined, sets:new()}, Graph).

-define(WALL(X, Y), sets:is_element({X, Y}, Walls)).

astar(Cost, States, Seen, {EX, EY} = End, Walls) ->
    L = maps:get(Cost, States, []),
    G = fun({{NX, NY} = NXY, NDXY, Delta}, {StatesAcc, SeenAcc} = GAcc) ->
                NState = {NXY, NDXY},
                case sets:is_element(NState, SeenAcc) orelse ?WALL(NX, NY) of
                    true ->
                        GAcc;
                    false ->
                        H = fun(V) -> [NState | V] end,
                        {maps:update_with(Cost + Delta, H, [NState], StatesAcc),
                         sets:add_element(NState, SeenAcc)}
                end
        end,
    F = fun({{X, Y}, _}, _) when X =:= EX, Y =:= EY ->
                Cost;
           ({{X, Y} = XY, {DX, DY} = D}, {_, _} = FAcc) ->
                Nexts = [{{X + DX, Y + DY}, D, 1},
                         {XY, lturn(D), 1000},
                         {XY, rturn(D), 1000}],
                lists:foldl(G, FAcc, Nexts);
           (_, Res) ->
                Res
        end,
    case lists:foldl(F, {maps:remove(Cost, States), Seen}, L) of
        {NewStates, NewSeen} ->
            astar(Cost + 1, NewStates, NewSeen, End, Walls);
        Res ->
            Res
    end.

lturn(?EAST ) -> ?NORTH;
lturn(?NORTH) -> ?WEST;
lturn(?WEST ) -> ?SOUTH;
lturn(?SOUTH) -> ?EAST.

rturn(?EAST ) -> ?SOUTH;
rturn(?SOUTH) -> ?WEST;
rturn(?WEST ) -> ?NORTH;
rturn(?NORTH) -> ?EAST.
