-module(aoc_2021_11_2).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    F = fun(_, Acc) ->
                {ok, [L]} = io:fread("", "~s"),
                Arr = array:from_list([X - $0 || X <- L]),
                array:set(array:size(Acc), Arr, Acc)
        end,
    lists:foldl(F, array:new(), lists:seq(1, 10)).

do(X) ->
    X.

acc(Acc, X) ->
    Acc + X.

fin(Graph) ->
    sim(Graph, 0).

sim(Graph, Steps) ->
    {Graph2, L} = inc(Graph),
    {Graph3, Seen} = bfs(queue:from_list(L), sets:from_list(L), Graph2),
    case sets:size(Seen) of
        100 ->
            Steps + 1;
        _ ->
            sim(update(Graph3, Seen), Steps + 1)
    end.

inc(Graph) ->
    F = fun(Y, Arr, {GraphAcc, LAcc}) ->
                G = fun(X, 9, {ArrAcc, GLAcc}) ->
                            {array:set(array:size(ArrAcc), 9, ArrAcc),
                             [{X, Y} | GLAcc]};
                       (_, V, {ArrAcc, GLAcc}) ->
                            {array:set(array:size(ArrAcc), V + 1, ArrAcc),
                             GLAcc}
                    end,
                {NewArr, NewL} = array:foldl(G, {array:new(), LAcc}, Arr),
                {array:set(array:size(GraphAcc), NewArr, GraphAcc), NewL}
        end,
    array:foldl(F, {array:new(), []}, Graph).

bfs(Q, Seen, Graph) ->
    case queue:out(Q) of
        {empty, _} ->
            {Graph, Seen};
        {{value, {X, Y}}, Q2} ->
            D = lists:seq(-1, 1),
            Neighbors = [{X + DX, Y + DY}
                         || DX <- D, DY <- D, DX =/= 0 orelse DY =/= 0],
            F = fun({NX, NY} = NXY, {QAcc, GAcc, SAcc} = Acc)
                      when NX >= 0, NX < 10, NY >= 0, NY < 10 ->
                        case sets:is_element(NXY, SAcc) of
                            true ->
                                Acc;
                            false ->
                                case arr(NX, NY, GAcc) of
                                    9 ->
                                        NQ = queue:in(NXY, QAcc),
                                        NS = sets:add_element(NXY, SAcc),
                                        {NQ, GAcc, NS};
                                    V ->
                                        NG = arr_set(NX, NY, V + 1, GAcc),
                                        {QAcc, NG, SAcc}
                                end
                        end;
                   (_, Acc) ->
                        Acc
                end,
            {NQ, NGraph, NSeen} = lists:foldl(F, {Q2, Graph, Seen}, Neighbors),
            bfs(NQ, NSeen, NGraph)
    end.

arr(X, Y, G) ->
    array:get(X, array:get(Y, G)).

arr_set(X, Y, V, G) ->
    A = array:get(Y, G),
    array:set(Y, array:set(X, V, A), G).

update(Graph, Seen) ->
    F = fun({X, Y}, Acc) ->
                arr_set(X, Y, 0, Acc)
        end,
    sets:fold(F, Graph, Seen).
