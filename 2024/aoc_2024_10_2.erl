-module(aoc_2024_10_2).

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
            array:from_list([S - $0 || S <- X])
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(TAB, memo).

fin(Graph) ->
    ets:new(?TAB, [named_table]),
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, 9, GAcc) ->
                            ets:insert(?TAB, {{X, Y}, 1}),
                            GAcc;
                       (X, H, GAcc) ->
                            case H of
                                0 ->
                                    GAcc + dp(X, Y, H, Graph);
                                _ ->
                                    GAcc
                            end
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, 0, Graph).

-define(D, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).

-define(ia(X, Y, H), (X >= 0 andalso X < array:size(array:get(0, Graph))
                      andalso Y >= 0 andalso Y < array:size(Graph)
                      andalso array:get(X, array:get(Y, Graph)) =:= H)).

dp(_, _, 9, _) ->
    1;
dp(X, Y, H, Graph) ->
    case ets:lookup_element(?TAB, {X, Y}, 2, undefined) of
        undefined ->
            F = fun({DX, DY}, Acc) ->
                        NX = X + DX,
                        NY = Y + DY,
                        case ?ia(NX, NY, H + 1) of
                            true ->
                                Acc + dp(NX, NY, H + 1, Graph);
                            false ->
                                Acc
                        end
                end,
            Res = lists:foldl(F, 0, ?D),
            ets:insert(?TAB, {{X, Y}, Res}),
            Res;
        Res ->
            Res
    end.
