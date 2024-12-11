-module(aoc_2024_10_1).

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
                            XY = {X, Y},
                            ets:insert(?TAB, {XY, sets:from_list([XY])}),
                            GAcc;
                       (X, H, GAcc) ->
                            case H of
                                0 ->
                                    GAcc + sets:size(dp(X, Y, H, Graph));
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

dp(X, Y, 9, _) ->
    sets:from_list([{X, Y}]);
dp(X, Y, H, Graph) ->
    case ets:lookup_element(?TAB, {X, Y}, 2, undefined) of
        undefined ->
            F = fun({DX, DY}, Acc) ->
                        NX = X + DX,
                        NY = Y + DY,
                        case ?ia(NX, NY, H + 1) of
                            true ->
%%                                io:format("dp(~p,~p): ~p unions~n", [X, Y, sets:to_list(Acc)]),
                                sets:union(Acc, dp(NX, NY, H + 1, Graph));
                            false ->
                                Acc
                        end
                end,
            Res = lists:foldl(F, sets:new(), ?D),
%%            io:format("dp(~p,~p)=~p~n", [X, Y, sets:to_list(Res)]),
            ets:insert(?TAB, {{X, Y}, Res}),
            Res;
        Res ->
            Res
    end.
