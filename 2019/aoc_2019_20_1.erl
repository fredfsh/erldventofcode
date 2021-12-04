-module(aoc_2019_20_1).

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
    {SXY, TXY, Portals} = parse_graph(X),
    bfs(SXY, TXY, Portals, X).

parse_graph(Graph) ->
    parse_graph_impl(undefined, undefined, maps:new(), Graph).

parse_graph_impl(SXY, TXY, Portals, Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $., {SAcc, TAcc, PAcc} = Acc) ->
                            case name(X, Y, Graph) of
                                undefined ->
                                    Acc;
                                "AA" ->
                                    {{X, Y}, TAcc, PAcc};
                                "ZZ" ->
                                    {SAcc, {X, Y}, PAcc};
                                Name ->
                                    {SAcc, TAcc, portal(Name, {X, Y}, PAcc)}
                            end;
                       (_, _, Acc) ->
                            Acc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, {SXY, TXY, Portals}, Graph).

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

portal(Name, XY, Portals) ->
    case maps:get(Name, Portals, undefined) of
        undefined ->
            maps:put(Name, XY, Portals);
        Z ->
            maps:put(XY, Z, maps:put(Z, XY, maps:remove(Name, Portals)))
    end.

bfs(SXY, TXY, Portals, Graph) ->
    Q = queue:from_list([{SXY, 0}]),
    Visited = sets:from_list([SXY]),
    bfs_impl(Q, Visited, TXY, Portals, Graph).

-define(D2, [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]).

bfs_impl(Q, Visited, TXY, Portals, Graph) ->
    {{value, {{X, Y}, N}}, Q2} = queue:out(Q),
    Neighbors = [{X + DX, Y + DY} || {DX, DY} <- ?D2],
    Nexts = case maps:get({X, Y}, Portals, undefined) of
                undefined ->
                    Neighbors;
                PXY ->
                    [PXY | Neighbors]
            end,
    F = fun(NXY, _) when NXY =:= TXY ->
                N + 1;
           (_, Res) when is_integer(Res) ->
                Res;
           ({NX, NY} = NXY, {QAcc, VAcc} = Acc) ->
                case (not sets:is_element(NXY, VAcc))
                    andalso ?G(NX, NY) =:= $. of
                    true ->
                        {queue:in({NXY,N+1}, QAcc), sets:add_element(NXY, VAcc)};
                    false ->
                        Acc
                end
        end,
    case lists:foldl(F, {Q2, Visited}, Nexts) of
        Res when is_integer(Res) ->
            Res;
        {NewQ, NewVisited} ->
            bfs_impl(NewQ, NewVisited, TXY, Portals, Graph)
    end.
