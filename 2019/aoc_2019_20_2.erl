-module(aoc_2019_20_2).

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
            string:trim(L, trailing, "\n")
    end.

-define(SPACE, 16#20).

ini() ->
    array:new({default, array:new({default, ?SPACE})}).

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), array:from_list(X), Acc).

fin(X) ->
    {S, T, Portals} = parse_graph(X),
    bfs(S, T, Portals, X).

parse_graph(Graph) ->
    parse_graph_impl(undefined, undefined, maps:new(), Graph).

parse_graph_impl(S, T, Portals, Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $., {SAcc, TAcc, PAcc} = Acc) ->
                            case name(X, Y, Graph) of
                                undefined ->
                                    Acc;
                                "AA" ->
                                    {{{X, Y}, 0}, TAcc, PAcc};
                                "ZZ" ->
                                    {SAcc, {{X, Y}, 0}, PAcc};
                                Name ->
                                    NPAcc = portal(Name, {X, Y}, PAcc, Graph),
                                    {SAcc, TAcc, NPAcc}
                            end;
                       (_, _, Acc) ->
                            Acc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, {S, T, Portals}, Graph).

-define(D, [{-2,  0, -1,  0},
            { 0, -2,  0, -1},
            { 1,  0,  2,  0},
            { 0,  1,  0,  2}]).

-define(G(X, Y), array:get(X, array:get(Y, Graph))).

name(X, Y, Graph) ->
    Cross = [[?G(X+DX0, Y+DY0), ?G(X+DX1, Y+DY1)] || {DX0, DY0, DX1, DY1} <- ?D],
    F = fun([A, B]) ->
                A >= $A andalso A =< $Z andalso B >= $A andalso B =< $Z
        end,
    case lists:filter(F, Cross) of
        [] ->
            undefined;
        [Name] ->
            Name
    end.

portal(Name, {X, Y} = XY, Portals, Graph) ->
    YMax = array:size(Graph) - 1,
    H = fun(_, Arr, Acc) ->
                max(Acc, array:size(Arr))
        end,
    XMax = array:foldl(H, 0, Graph) - 1,
    DDepth = case {min(X, Y), XMax - 2, YMax - 2} of
                 {2, _, _} ->
                     -1;
                 {_, X, _} ->
                     -1;
                 {_, _, Y} ->
                     -1;
                 _ ->
                     1
             end,
    case maps:get(Name, Portals, undefined) of
        undefined ->
            maps:put(Name, {XY, DDepth}, Portals);
        {Z, DD} ->
            M = maps:remove(Name, Portals),
            maps:put(XY, {Z, -DD}, maps:put(Z, {XY, DD}, M))
    end.

bfs(S, T, Portals, Graph) ->
    Q = queue:from_list([{S, 0}]),
    Visited = sets:from_list([S]),
    bfs_impl(Q, Visited, T, Portals, Graph).

-define(D2, [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]).

bfs_impl(Q, Visited, T, Portals, Graph) ->
    {{value, {{{X, Y}, D}, N}}, Q2} = queue:out(Q),
    Neighbors = [{{X + DX, Y + DY}, D} || {DX, DY} <- ?D2],
    Nexts = case maps:get({X, Y}, Portals, undefined) of
                undefined ->
                    Neighbors;
                {_, -1} when D =:= 0 ->
                    Neighbors;
                {PXY, DD} ->
                    [{PXY, D + DD} | Neighbors]
            end,
    F = fun(Next, _) when Next =:= T ->
                N + 1;
           (_, Res) when is_integer(Res) ->
                Res;
           ({{NX, NY}, _} = Next, {QAcc, VAcc} = Acc) ->
                case (not sets:is_element(Next, VAcc))
                    andalso ?G(NX, NY) =:= $. of
                    true ->
                        {queue:in({Next, N + 1}, QAcc),
                         sets:add_element(Next, VAcc)};
                    false ->
                        Acc
                end
        end,
    case lists:foldl(F, {Q2, Visited}, Nexts) of
        Res when is_integer(Res) ->
            Res;
        {NewQ, NewVisited} ->
            bfs_impl(NewQ, NewVisited, T, Portals, Graph)
    end.
