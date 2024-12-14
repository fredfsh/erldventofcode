-module(aoc_2024_12_1).

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

floodfill_impl(Q, Visited, Area, Perimeter, Plant, Graph) ->
    case queue:out(Q) of
        {empty, _} ->
%%            io:format("~p * ~p~n", [Area, Perimeter]),
            {Area * Perimeter, Visited};
        {{value, {X, Y}}, Q2} ->
            F = fun({DX, DY}, {QAcc, VisitedAcc, AreaAcc, PerimeterAcc}) ->
                        {NX, NY} = NXY = {X + DX, Y + DY},
                        case NX < 0 orelse NX >= ?COLS orelse
                            NY < 0 orelse NY >= ?ROWS of
                            true ->
                                {QAcc, VisitedAcc, AreaAcc, PerimeterAcc + 1};
                            false ->
                                NPerimeter = case ?a(NX, NY) of
                                                 Plant ->
                                                     PerimeterAcc;
                                                 _ ->
                                                     PerimeterAcc + 1
                                             end,
                                case {sets:is_element({NX, NY}, VisitedAcc),
                                      ?a(NX, NY)} of
                                    {false, Plant} ->
                                        {queue:in(NXY, QAcc),
                                         sets:add_element(NXY, VisitedAcc),
                                         AreaAcc + 1,
                                         PerimeterAcc};
                                    _ ->
                                        {QAcc, VisitedAcc, AreaAcc, NPerimeter}
                                end
                        end
                end,
            {NQ, NVisited, NArea, NPerimeter} =
                lists:foldl(F, {Q2, Visited, Area, Perimeter}, ?D),
            floodfill_impl(NQ, NVisited, NArea, NPerimeter, Plant, Graph)
    end.
