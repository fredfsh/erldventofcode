-module(aoc_2018_3_1).

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
    case io:fread("", "#~d @ ~d,~d: ~dx~d") of
        eof ->
            eof;
        {ok, [_, X, Y, W, H]} ->
            {X, Y, W, H}
    end.

-define(N, 1000).

ini() ->
    array:new(?N, {default, array:new(?N, {default, 0})}).

do(X) ->
    X.

acc(Arr, {X, Y, W, H}) ->
    G = fun(I, Acc) ->
                array:set(I, array:get(I, Acc) + 1, Acc)
        end,
    F = fun(I, Acc) ->
                Array = array:get(I, Acc),
                array:set(I, lists:foldl(G, Array, lists:seq(X, X + W - 1)), Acc)
        end,
    lists:foldl(F, Arr, lists:seq(Y, Y + H - 1)).

fin(X) ->
    G = fun(_, N, Acc) when N >= 2 ->
                Acc + 1;
           (_, _, Acc) ->
                Acc
        end,
    F = fun(_, Arr, Acc) ->
                array:foldl(G, Acc, Arr)
        end,
    array:foldl(F, 0, X).
