-module(aoc_2024_12_2).

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

fin(Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, _, {PriceAcc, Visited}) ->
                            {Price, NVisited} = floodfill(Visited, X, Y, Graph),
%%                            io:format("floodfill(~p,~p) = ~p~n~p~n", [X, Y, Price, NVisited]),
                            {PriceAcc + Price, NVisited}
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    {Res, _} = array:foldl(F, {0, sets:new()}, Graph),
    Res.

-define(a(X, Y), array:get(X, array:get(Y, Graph))).

floodfill(Visited, X, Y, Graph) ->
    case sets:is_element({X, Y}, Visited) of
        true ->
            {0, Visited};
        false ->
            Plant = ?a(X, Y),
            Q = queue:from_list([{X, Y}]),
            NVisited = sets:add_element({X, Y}, Visited),
            floodfill_impl(Q, NVisited, 1, 0, Plant, Graph)
    end.

-define(D, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).

-define(COLS, array:size(array:get(0, Graph))).
-define(ROWS, array:size(Graph)).

floodfill_impl(Q, Visited, Area, Sides, Plant, Graph) ->
    case queue:out(Q) of
        {empty, _} ->
%%            io:format("~p * ~p~n", [Area, Sides]),
            {Area * Sides, Visited};
        {{value, {X, Y}}, Q2} ->
            F = fun({DX, DY}, {QAcc, VisitedAcc, AreaAcc, SidesAcc}) ->
                        {NX, NY} = NXY = {X + DX, Y + DY},
                        NewSide = new_side(NX, NY, DX, DY, Graph),
                        case NX < 0 orelse NX >= ?COLS orelse
                            NY < 0 orelse NY >= ?ROWS of
                            true ->
                                %% case NewSide of
                                %%     1 ->
                                %%         io:format("+1 new side: ~p,~p -> ~p,~p~n", [X, Y, NX, NY]);
                                %%     0 ->
                                %%         ok
                                %% end,
                                {QAcc, VisitedAcc, AreaAcc, SidesAcc + NewSide};
                            false ->
                                NSides = case ?a(NX, NY) of
                                             Plant ->
                                                 SidesAcc;
                                             _ ->
                                                 %% case NewSide of
                                                 %%     1 ->
                                                 %%         io:format("+1 new side: ~p,~p -> ~p,~p~n", [X, Y, NX, NY]);
                                                 %%     0 ->
                                                 %%         ok
                                                 %% end,
                                                 SidesAcc + NewSide
                                         end,
                                case {sets:is_element({NX, NY}, VisitedAcc),
                                      ?a(NX, NY)} of
                                    {false, Plant} ->
                                        {queue:in(NXY, QAcc),
                                         sets:add_element(NXY, VisitedAcc),
                                         AreaAcc + 1,
                                         SidesAcc};
                                    _ ->
                                        {QAcc, VisitedAcc, AreaAcc, NSides}
                                end
                        end
                end,
            {NQ, NVisited, NArea, NSides} =
                lists:foldl(F, {Q2, Visited, Area, Sides}, ?D),
            floodfill_impl(NQ, NVisited, NArea, NSides, Plant, Graph)
    end.

new_side(-1, 0, -1, 0, _) ->
    1;
new_side(-1, Y, -1, 0, Graph) ->
    case ?a(0, Y) =:= ?a(0, Y - 1) of
        true ->
            0;
        false ->
            1
    end;
new_side(0, -1, 0, -1, _) ->
    1;
new_side(X, -1, 0, -1, Graph) ->
    case ?a(X, 0) =:= ?a(X - 1, 0) of
        true ->
            0;
        false ->
            1
    end;
new_side(X, Y, DX, DY, Graph) ->
%%    io:format("new side(~p ~p - ~p ~p)~n", [X, Y, DX, DY]),
    case {?COLS, ?ROWS} of
        {X, _} when Y =:= 0 ->
            1;
        {X, _} ->
            case ?a(?COLS - 1, Y - 1) =:= ?a(?COLS - 1, Y) of
                true ->
                    0;
                false ->
                    1
            end;
        {_, Y} when X =:= 0 ->
            1;
        {_, Y} ->
            case ?a(X - 1, ?ROWS - 1) =:= ?a(X, ?ROWS - 1) of
                true ->
                    0;
                false ->
                    1
            end;
        {_, _} when DY =:= 0, Y =:= 0 ->
            1;
        {_, _} when DY =:= 0 ->
            OX = X - DX,
            case ?a(OX, Y - 1) =/= ?a(OX, Y) of
                true ->
                    1;
                false ->
                    case ?a(OX, Y - 1) =:= ?a(X, Y - 1) of
                        true ->
                            1;
                        false ->
                            0
                    end
            end;
        {_, _} when DX =:= 0, X =:= 0 ->
            1;
        {_, _} when DX =:= 0 ->
            OY = Y - DY,
            case ?a(X - 1, OY) =/= ?a(X, OY) of
                true ->
                    1;
                false ->
                    case ?a(X - 1, OY) =:= ?a(X - 1, Y) of
                        true ->
                            1;
                        false ->
                            0
                    end
            end
    end.
