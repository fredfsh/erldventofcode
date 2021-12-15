-module(aoc_2021_15_2).

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
            array:from_list([X - $0 || X <- S])
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(G) ->
    G2 = expand(G),
    Y = array:size(G2),
    X = array:size(array:get(0, G2)),
    Seen = sets:from_list([{0, 0}]),
    bfs(0, maps:from_list([{0, Seen}]), Seen, X - 1, Y - 1, G2).

expand(G) ->
    expand_y(expand_x(G)).

-define(N, 5).

expand_x(A) ->
    F = fun(_, Arr) ->
                G = fun(D, Acc) ->
                            H = fun(_, X, ArrAcc) ->
                                        I = array:size(ArrAcc),
                                        array:set(I, inc(X, D), ArrAcc)
                                end,
                            array:foldl(H, Acc, Arr)
                    end,
                lists:foldl(G, Arr, lists:seq(1, ?N - 1))
        end,
    array:map(F, A).

inc(X, D) when is_integer(X) ->
    (X - 1 + D) rem 9 + 1;
inc(A, D) ->
    array:map(fun(_, X) -> inc(X, D) end, A).

expand_y(A) ->
    F = fun(D, FAcc) ->
                G = fun(_, Arr, GAcc) ->
                            array:set(array:size(GAcc), inc(Arr, D), GAcc)
                    end,
                array:foldl(G, FAcc, A)
        end,
    lists:foldl(F, A, lists:seq(1, ?N - 1)).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

bfs(I, Maps, Seen, TX, TY, Graph) ->
    case maps:take(I, Maps) of
        error ->
            bfs(I + 1, Maps, Seen, TX, TY, Graph);
        {Set, Maps2} ->
            F = fun({X, Y}, _) when X =:= TX, Y =:= TY ->
                        I;
                   (_, Risk) when is_integer(Risk) ->
                        Risk;
                   ({X, Y}, Acc) ->
                        Neighbors = [{X + DX, Y + DY} || {DX, DY} <- ?D],
                        G = fun({NX, NY}, GAcc)
                                  when NX < 0; NX > TX; NY < 0; NY > TY ->
                                    GAcc;
                               ({NX, NY} = NXY, {MA, SAcc} = GAcc) ->
                                    case sets:is_element(NXY, SAcc) of
                                        true ->
                                            GAcc;
                                        false ->
                                            V = I + arr(NX, NY, Graph),
                                            H = fun(S) ->
                                                        sets:add_element(NXY, S)
                                                end,
                                            O = sets:from_list([NXY]),
                                            NMap = maps:update_with(V, H, O, MA),
                                            {NMap, SAcc}
                                    end
                            end,
                        lists:foldl(G, Acc, Neighbors)
                end,
            case sets:fold(F, {Maps2, sets:union(Seen, Set)}, Set) of
                {NewMap, NewSeen} ->
                    bfs(I + 1, NewMap, NewSeen, TX, TY, Graph);
                Risk ->
                    Risk
            end
    end.

arr(X, Y, G) ->
    array:get(X, array:get(Y, G)).
