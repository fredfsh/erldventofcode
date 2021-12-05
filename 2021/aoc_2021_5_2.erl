-module(aoc_2021_5_2).

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
    case io:fread("", "~d,~d -> ~d,~d") of
        eof ->
            eof;
        {ok, X} ->
            X
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, [X1, Y1, X2, Y2]) when X1 =:= X2 ->
    F = fun(Y, FAcc) -> inc(FAcc, X1, Y) end,
    lists:foldl(F, Acc, lists:seq(min(Y1, Y2), max(Y1, Y2)));
acc(Acc, [X1, Y1, X2, Y2]) when Y1 =:= Y2 ->
    F = fun(X, FAcc) -> inc(FAcc, X, Y1) end,
    lists:foldl(F, Acc, lists:seq(min(X1, X2), max(X1, X2)));
acc(Acc, [X1, Y1, X2, Y2]) ->
    DX = (X2 - X1) div abs(X2 - X1),
    DY = (Y2 - Y1) div abs(Y2 - Y1),
    F = fun(I, FAcc) ->
                X = X1 + DX * I,
                Y = Y1 + DY * I,
                inc(FAcc, X, Y)
        end,
    lists:foldl(F, Acc, lists:seq(0, (X2 - X1) div DX)).

inc(Map, X, Y) ->
    maps:update_with({X, Y}, fun(V) -> V + 1 end, 1, Map).

fin(X) ->
    F = fun(_, V) -> V >= 2 end,
    maps:size(maps:filter(F, X)).
