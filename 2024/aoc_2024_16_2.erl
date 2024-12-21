-module(aoc_2024_16_2).

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

-define(TAB, prefix).

fin(Graph) ->
    {Start, End, Walls} = graph(Graph),
    Init = {Start, ?EAST},
    ets:new(?TAB, [named_table]),
    ets:insert_new(?TAB, {Init, 0}),
    Cost = astar(0, maps:from_list([{0, [Init]}]), End, Walls),
%%    io:format("cost: ~p~n", [Cost]),
    seats(Start, End, Walls, Cost).

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

astar(Cost, States, {EX, EY} = End, Walls) ->
    L = maps:get(Cost, States, []),
    G = fun({{NX, NY} = NXY, NDXY, Delta}, GAcc) ->
                NState = {NXY, NDXY},
                case {?WALL(NX, NY),
                      ets:lookup_element(?TAB, NState, 2, undefined)} of
                    {false, undefined} ->
%%                        io:format("[forward] ~p = ~p~n", [NState, Cost + Delta]),
                        ets:insert_new(?TAB, {NState, Cost + Delta}),
                        H = fun(V) -> [NState | V] end,
                        maps:update_with(Cost + Delta, H, [NState], GAcc);
                    _ ->
                        GAcc
                end
        end,
    F = fun({{X, Y}, _}, _) when X =:= EX, Y =:= EY ->
                Cost;
           ({{X, Y} = XY, {DX, DY} = D}, FAcc) when is_map(FAcc) ->
                Nexts = [{{X + DX, Y + DY}, D, 1},
                         {XY, lturn(D), 1000},
                         {XY, rturn(D), 1000}],
                lists:foldl(G, FAcc, Nexts);
           (_, Res) ->
                Res
        end,
    case lists:foldl(F, maps:remove(Cost, States), L) of
        NewStates when is_map(NewStates) ->
            astar(Cost + 1, NewStates, End, Walls);
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

seats(Start, End, Walls, Best) ->
    Inits = [{End, ?EAST}, {End, ?NORTH}, {End, ?WEST}, {End, ?SOUTH}],
    Seen = sets:from_list(Inits),
    States = maps:from_list([{0, Inits}]),
    seats_impl(sets:from_list([End]), 0, States, Seen, Start, Walls, Best).

seats_impl(Seats, Cost, _, _, _, _, Best) when Cost =:= Best + 1 ->
%%    print(Seats, Walls),
    sets:size(Seats);
seats_impl(Seats, Cost, States, Seen, {SX, SY} = Start, Walls, Best) ->
    L = maps:get(Cost, States, []),
    G = fun({{NX,NY}=NXY, NDXY, Delta}, {SeatsAcc,StatesAcc,SeenAcc}=GAcc) ->
                NState = {NXY, NDXY},
                case ?WALL(NX, NY) orelse sets:is_element(NState, SeenAcc) of
                    true ->
                        GAcc;
                    false ->
                        Forward = ets:lookup_element(?TAB, NState, 2, Best + 1),
                        NewSeats =
                            case Forward + Cost + Delta of
                                Best ->
                                    sets:add_element(NXY, SeatsAcc);
                                _ ->
                                    SeatsAcc
                            end,
                        H = fun(V) -> [NState | V] end,
                        Key = Cost + Delta,
                        Init = [NState],
                        NewStates = maps:update_with(Key, H, Init, StatesAcc),
                        {NewSeats, NewStates, sets:add_element(NState, SeenAcc)}
                end
        end,
    F = fun({{X, Y}, _}, FAcc) when X =:= SX, Y =:= SY ->
                FAcc;
           ({{X, Y} = XY, {DX, DY} = D}, FAcc) ->
                Nexts = [{{X - DX, Y - DY}, D, 1},
                         {XY, lturn(D), 1000},
                         {XY, rturn(D), 1000}],
                lists:foldl(G, FAcc, Nexts)
        end,
    Init = {Seats, maps:remove(Cost, States), Seen},
    {NewSeats, NewStates, NewSeen} = lists:foldl(F, Init, L),
    seats_impl(NewSeats, Cost + 1, NewStates, NewSeen, Start, Walls, Best).

-define(SEAT(X, Y), sets:is_element({X, Y}, Seats)).

%% print(Seats, Walls) ->
%%     {Xs, Ys} = lists:unzip(sets:to_list(Walls)),
%%     {MX, MY} = {lists:max(Xs), lists:max(Ys)},
%%     FY = fun(Y) ->
%%                  FX = fun(X) ->
%%                               C = case {?WALL(X, Y), ?SEAT(X, Y)} of
%%                                       {true, false} ->
%%                                           $#;
%%                                       {false, true} ->
%%                                           $O;
%%                                       {false, false} ->
%%                                           $.
%%                                   end,
%%                               io:format("~c", [C])
%%                       end,
%%                  lists:foreach(FX, lists:seq(0, MX)),
%%                  io:format("~n")
%%          end,
%%     lists:foreach(FY, lists:seq(0, MY)).
