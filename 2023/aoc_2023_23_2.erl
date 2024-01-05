-module(aoc_2023_23_2).

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
        {ok, [S]} ->
            array:from_list(S)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Matrix) ->
    {Start, Stop, Vertices} = vertices(Matrix),
    %%io:format("~p~n", [sets:to_list(Vertices)]),
    Edges = edges(Vertices, Matrix),
    %%io:format("~p~n", [Edges]),
    dfs(Start, Stop, Edges).

-define(Ds, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).
-define(a(X, Y), array:get(X, array:get(Y, Matrix))).

vertices(Matrix) ->
    Start = {path(0, array:get(0, Matrix)), 0},
    Rows = array:size(Matrix),
    Stop = {path(0, array:get(Rows - 1, Matrix)), Rows - 1},
    Cols = array:size(array:get(0, Matrix)),
    H = fun({NX, NY}) when NX < 0; NX >= Cols; NY < 0; NY >= Rows ->
                false;
           ({NX, NY}) ->
                ?a(NX, NY) =/= $#
        end,
    F = fun(Y, Arr, FAcc) ->
                G = fun(_, $#, GAcc) ->
                            GAcc;
                       (X, _, GAcc) ->
                            L = [{X + DX, Y + DY} || {DX, DY} <- ?Ds],
                            case length(lists:filter(H, L)) >= 3 of
                                true ->
                                    sets:add_element({X, Y}, GAcc);
                                false ->
                                    GAcc
                            end
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    {Start, Stop, array:foldl(F, sets:from_list([Start, Stop]), Matrix)}.

path(X, Arr) ->
    case array:get(X, Arr) of
        $. ->
            X;
        _ ->
            path(X + 1, Arr)
    end.

edges(Vertices, Matrix) ->
    L = sets:to_list(Vertices),
    maps:from_list([{V, es(V, Vertices, Matrix)} || V <- L]).

es(Vertex, Vertices, Matrix) ->
    es_impl(maps:new(), 0, Vertex, sets:from_list([Vertex]), Vertices, Matrix).

es_impl(Edges, Len, XY, Visited, Vertices, Matrix) ->
    case sets:is_element(XY, Vertices) of
        true when Len > 0 ->
            maps:put(XY, Len, Edges);
        _ ->
            F = fun(NXY, Acc) ->
                        NewVisited = sets:add_element(NXY, Visited),
                        es_impl(Acc, Len + 1, NXY, NewVisited, Vertices, Matrix)
                end,
            lists:foldl(F, Edges, nexts(XY, Visited, Matrix))
    end.

-define(VISITED(X, Y), sets:is_element({X, Y}, Visited)).

nexts({X, Y}, Visited, Matrix) ->
    Rows = array:size(Matrix),
    Cols = array:size(array:get(0, Matrix)),
    F = fun({DX, DY}, Acc) ->
                case {X + DX, Y + DY} of
                    {NX, NY} when NX < 0; NX >= Cols; NY < 0; NY >= Rows ->
                        Acc;
                    {NX, NY} = NXY ->
                        case {?VISITED(NX, NY), ?a(NX, NY)} of
                            {true, _} ->
                                Acc;
                            {false, $#} ->
                                Acc;
                            {false, _} ->
                                [NXY | Acc]
                        end
                end
        end,
    lists:foldl(F, [], ?Ds).

dfs(Start, Stop, Edges) ->
    dfs_impl(0, Start, sets:from_list([Start]), Stop, Edges).

dfs_impl(Acc, Goal, _, Goal, _) ->
    Acc;
dfs_impl(Acc, Vertex, Visited, Goal, Edges) ->
    Nexts = maps:get(Vertex, Edges),
    F = fun(Next, Cost, FAcc) ->
                case sets:is_element(Next, Visited) of
                    true ->
                        FAcc;
                    false ->
                        NewCost = Acc + Cost,
                        NewVisited = sets:add_element(Next, Visited),
                        Res = dfs_impl(NewCost, Next, NewVisited, Goal, Edges),
                        max(FAcc, Res)
                end
        end,
    maps:fold(F, 0, Nexts).
