-module(aoc_2018_13_2).

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
    case io:get_line("") of
        eof ->
            eof;
        L ->
            string:trim(L, trailing)
    end.

ini() ->
    {0, maps:new(), []}.

do(X) ->
    X.

acc({Y, Graph, Carts}, L) ->
    F = fun($|, {X, GraphAcc, CartsAcc}) ->
                {X + 1, maps:put({X, Y}, road, GraphAcc), CartsAcc};
           ($-, {X, GraphAcc, CartsAcc}) ->
                {X + 1, maps:put({X, Y}, road, GraphAcc), CartsAcc};
           ($/, {X, GraphAcc, CartsAcc}) ->
                {X + 1, maps:put({X, Y}, nesw, GraphAcc), CartsAcc};
           ($\\, {X, GraphAcc, CartsAcc}) ->
                {X + 1, maps:put({X, Y}, nwse, GraphAcc), CartsAcc};
           ($+, {X, GraphAcc, CartsAcc}) ->
                {X + 1, maps:put({X, Y}, cross, GraphAcc), CartsAcc};
           ($^, {X, GraphAcc, CartsAcc}) ->
                {X + 1,
                 maps:put({X, Y}, road, GraphAcc),
                 [{Y, X, 0, -1, left} | CartsAcc]};
           ($v, {X, GraphAcc, CartsAcc}) ->
                {X + 1,
                 maps:put({X, Y}, road, GraphAcc),
                 [{Y, X, 0, 1, left} | CartsAcc]};
           ($<, {X, GraphAcc, CartsAcc}) ->
                {X + 1,
                 maps:put({X, Y}, road, GraphAcc),
                 [{Y, X, -1, 0, left} | CartsAcc]};
           ($>, {X, GraphAcc, CartsAcc}) ->
                {X + 1,
                 maps:put({X, Y}, road, GraphAcc),
                 [{Y, X, 1, 0, left} | CartsAcc]};
           (_, {X, GraphAcc, CartsAcc}) ->
                {X + 1, GraphAcc, CartsAcc}
        end,
    {_, NewGraph, NewCarts} = lists:foldl(F, {0, Graph, Carts}, L),
    {Y + 1, NewGraph, NewCarts}.

fin({_, Graph, Carts}) ->
    crash(lists:sort(Carts), Graph).

crash(Carts, Graph) ->
    Init = sets:from_list([{X, Y} || {Y, X, _, _, _} <- Carts]),
    move([], Init, Carts, Graph).

move([{Y, X, _, _, _}], _, [], _) ->
    {X, Y};
move(Acc, Set, [], Graph) ->
    move([], Set, lists:sort(Acc), Graph);
move(Acc, Set, [{Y, X, DX, DY, Action} | T], Graph) ->
    NX = X + DX,
    NY = Y + DY,
    Set2 = sets:del_element({X, Y}, Set),
    case sets:is_element({NX, NY}, Set2) of
        true ->
            NewAcc = remove_cart(NX, NY, Acc),
            NewSet = sets:del_element({NX, NY}, Set2),
            move(NewAcc, NewSet, remove_cart(NX, NY, T), Graph);
        false ->
            NewSet = sets:add_element({NX, NY}, Set2),
            {NDX, NDY, NewAction} = case maps:get({NX, NY}, Graph) of
                                        road ->
                                            {DX, DY, Action};
                                        cross ->
                                            cross(DX, DY, Action);
                                        Turn ->
                                            {TDX, TDY} = turn(DX, DY, Turn),
                                            {TDX, TDY, Action}
                                    end,
            move([{NY, NX, NDX, NDY, NewAction} | Acc], NewSet, T, Graph)
    end.

remove_cart(X, Y, Acc) ->
    F = fun({CY, CX, _, _, _}) ->
                CX =/= X orelse CY =/= Y
        end,
    lists:filter(F, Acc).

cross(DX, DY, straight) ->
    {DX, DY, right};
cross(DX, DY, left) ->
    {NDX, NDY} = turn_left(DX, DY),
    {NDX, NDY, straight};
cross(DX, DY, right) ->
    {NDX, NDY} = turn_right(DX, DY),
    {NDX, NDY, left}.

turn_left(1, 0) ->
    {0, -1};
turn_left(0, -1) ->
    {-1, 0};
turn_left(-1, 0) ->
    {0, 1};
turn_left(0, 1) ->
    {1, 0}.

turn_right(1, 0) ->
    {0, 1};
turn_right(0, 1) ->
    {-1, 0};
turn_right(-1, 0) ->
    {0, -1};
turn_right(0, -1) ->
    {1, 0}.

turn(DX, 0, nwse) ->
    turn_right(DX, 0);
turn(0, DY, nwse) ->
    turn_left(0, DY);
turn(DX, 0, nesw) ->
    turn_left(DX, 0);
turn(0, DY, nesw) ->
    turn_right(0, DY).
