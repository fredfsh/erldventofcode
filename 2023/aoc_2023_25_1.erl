-module(aoc_2023_25_1).

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
            Parts = string:split(string:trim(L), " ", all),
            [string:trim(P, trailing, ":") || P <- Parts]
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Edges, [H | T]) ->
    F = fun(V, Acc) -> add_edge(V, H, Acc) end,
    lists:foldl(F, Edges, T).

add_edge(V, U, Edges) ->
    F = fun(Vertex) ->
                fun(Vertices) ->
                        sets:add_element(Vertex, Vertices)
                end
        end,
    ResV = maps:update_with(V, F(U), sets:from_list([U]), Edges),
    ResU = maps:update_with(U, F(V), sets:from_list([V]), ResV),
    ResU.

-define(TAB, ranks).

%% For each edge E = {V, U}, find the shortest path from V to U assuming E is
%% not present in the graph.  Then, for each edge F on the path, by removing
%% edge E and edge F from the graph, see if there's only one critical edge
%% from V to U.  An edge is critical if it's not on a loop.
fin(Edges) ->
    ets:new(?TAB, [named_table]),
    F = fun(V, Us, undefined) ->
                G = fun(U, undefined) ->
                            Path = path(V, U, Edges),
                            bridge(Path, V, U, Edges);
                       (_, Res) ->
                            Res
                    end,
                sets:fold(G, undefined, Us);
           (_, _, Res) ->
                Res
        end,
    Bridges = maps:fold(F, undefined, Edges),
    mul(Bridges, Edges).

path(V, U, Edges) ->
    Q = queue:from_list([{V, []}]),
    Visited = sets:from_list([V]),
    path_impl(Q, Visited, U, del_edge(V, U, Edges)).

del_edge(V, U, Edges) ->
    F = fun(Vertex) ->
                fun(Vertices) ->
                        sets:del_element(Vertex, Vertices)
                end
        end,
    ResV = maps:update_with(V, F(U), Edges),
    ResU = maps:update_with(U, F(V), ResV),
    ResU.

path_impl(Q, Visited, Target, Edges) ->
    case queue:out(Q) of
        {{value, {Target, Acc}}, _} ->
            lists:reverse(Acc);
        {{value, {V, Path}}, Q2} ->
            F = fun(U, {QAcc, VisitedAcc} = Acc) ->
                        case sets:is_element(U, VisitedAcc) of
                            true ->
                                Acc;
                            false ->
                                {queue:in({U, [{V, U} | Path]}, QAcc),
                                 sets:add_element(U, VisitedAcc)}
                        end
                end,
            {NQ, NVisited} = sets:fold(F, {Q2, Visited}, maps:get(V, Edges)),
            path_impl(NQ, NVisited, Target, Edges)
    end.

bridge([], _, _, _) ->
    undefined;
bridge([{HV, HU} | T], V, U, Edges) ->
    Init = del_edge(V, U, del_edge(HV, HU, Edges)),
%%    io:format("bridging: {~p,~p}, {~p,~p}~n", [V, U, HV, HU]),
    case bridge_impl(V, Init) of
        undefined ->
            bridge(T, V, U, Edges);
        Edge ->
            [{V, U}, {HV, HU}, Edge]
    end.

%% https://www.geeksforgeeks.org/find-all-critical-connections-in-the-graph/
bridge_impl(V, Edges) ->
    ets:delete_all_objects(?TAB),
    ets:insert_new(?TAB, {V, 0}),
    {_, Res, _} = dfs(V, sets:new(), Edges),
    Res.

dfs(V, Visited, Edges) ->
%%    io:format("  dfsing ~p, ranks: ~p~n", [V, ets:tab2list(?TAB)]),
    RV = rank(V),
    F = fun(U, {MinRankAcc, undefined, VisitedAcc} = Acc) ->
                case sets:is_element({V, U}, VisitedAcc) of
                    true ->
%%                        io:format("    {~p,~p} visited~n", [V, U]),
                        Acc;
                    false ->
%%                        io:format("    {~p,~p} not visited~n", [V, U]),
%%                        io:format("    r(~p) = ~p~n", [U, rank(U)]),
                        NewVisited = visited(V, U, VisitedAcc),
                        case rank(U) of
                            undefined ->
                                ets:insert_new(?TAB, {U, ets:info(?TAB, size)}),
                                case dfs(U, NewVisited, Edges) of
                                    {Rank, _, NVisited} when Rank > RV ->
%%                                        io:format("      dfs(~p) returns ~p > ~p~n", [U, Rank, RV]),
                                        {MinRankAcc, {V, U}, NVisited};
                                    {MinRankU, Res, NVisited} ->
%%                                        io:format("      dfs(~p) returns ~p~n", [U, MinRankU]),
                                        NMinRank = min(MinRankAcc, MinRankU),
                                        {NMinRank, Res, NVisited}
                                end;
                            RU ->
%%                                io:format("      rank(~p) = ~p~n", [U, RU]),
                                {min(RU, MinRankAcc), undefined, NewVisited}
                        end
                end;
           (_, Acc) ->
                Acc
        end,
    sets:fold(F, {RV, undefined, Visited}, maps:get(V, Edges)).

rank(V) ->
    ets:lookup_element(?TAB, V, 2, undefined).

visited(V, U, Edges) ->
    sets:add_element({V, U}, sets:add_element({U, V}, Edges)).

mul([{V, _}, _, _] = Bridges, Edges) ->
    Size = floodfill(V, sets:from_list(Bridges), Edges),
    Size * (maps:size(Edges) - Size).

floodfill(V, Bridges, Edges) ->
    L = [V],
    floodfill_impl(queue:from_list(L), sets:from_list(L), Bridges, Edges).

floodfill_impl(Q, Seen, Bridges, Edges) ->
    case queue:out(Q) of
        {empty, _} ->
            sets:size(Seen);
        {{value, V}, Q2} ->
            F = fun(U, {QAcc, SeenAcc} = Acc) ->
                        case sets:is_element({V, U}, Bridges)
                            orelse sets:is_element(U, SeenAcc) of
                            true ->
                                Acc;
                            false ->
                                {queue:in(U,QAcc), sets:add_element(U,SeenAcc)}
                        end
                end,
            {NQ, NSeen} = sets:fold(F, {Q2, Seen}, maps:get(V, Edges)),
            floodfill_impl(NQ, NSeen, Bridges, Edges)
    end.
