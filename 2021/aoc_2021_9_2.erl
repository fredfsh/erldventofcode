-module(aoc_2021_9_2).

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
        {ok, [L]} ->
            array:from_list([X - $0 || X <- L])
    end.

ini() ->
    array:new({default, array:new({default, undefined})}).

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Graph) ->
    Basins = basins(Graph),
    [A, B, C | _] = lists:reverse(lists:sort(Basins)),
    A * B * C.

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

basins(Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(_, 9, GAcc) ->
                            GAcc;
                       (X, _, {BasinsAcc, SeenAcc} = GAcc) ->
                            case sets:is_element({X, Y}, SeenAcc) of
                                true ->
                                    GAcc;
                                false ->
                                    Area = bfs(X, Y, Graph),
                                    {[sets:size(Area) | BasinsAcc],
                                     sets:union(Area, SeenAcc)}
                            end
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    {Basins, _} = array:foldl(F, {[], sets:new()}, Graph),
    Basins.

bfs(X, Y, Graph) ->
    XY = {X, Y},
    bfs_impl(queue:from_list([XY]), sets:from_list([XY]), Graph).

bfs_impl(Q, Visited, Graph) ->
    case queue:out(Q) of
        {empty, _} ->
            Visited;
        {{value, {X, Y}}, Q2} ->
            Neighbors = [{X + DX, Y + DY} || {DX, DY} <- ?D],
            F = fun({NX, NY} = NXY, {QAcc, VisitedAcc} = Acc) ->
                        case NX >= 0
                            andalso NX < array:size(array:get(0, Graph))
                            andalso NY >= 0
                            andalso NY < array:size(Graph)
                            andalso (not sets:is_element(NXY, VisitedAcc))
                            andalso height(NX, NY, Graph) =/= 9 of
                            true ->
                                {queue:in(NXY, QAcc),
                                 sets:add_element(NXY, VisitedAcc)};
                            false ->
                                Acc
                        end
                end,
            {NewQ, NewVisited} = lists:foldl(F, {Q2, Visited}, Neighbors),
            bfs_impl(NewQ, NewVisited, Graph)
    end.

height(X, Y, Graph) ->
    array:get(X, array:get(Y, Graph)).
