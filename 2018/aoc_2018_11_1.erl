-module(aoc_2018_11_1).

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
    [].

do(X) ->
    Arr = fill(X),
    largest(Arr).

-define(N, 300).

fill(SN) ->
    F = fun(Y, Acc) ->
                G = fun(X, Acc2) ->
                            Rack = X + 10,
                            Power = (Rack * Y + SN) * Rack rem 1000 div 100 - 5,
                            array:set(X, Power, Acc2)
                    end,
                Arr = lists:foldl(G, array:new(?N + 1), lists:seq(1, ?N)),
                array:set(Y, Arr, Acc)
        end,
    lists:foldl(F, array:new(?N + 1), lists:seq(1, ?N)).

-define(D, 3).

largest(Arr) ->
    F = fun(Y, Acc) ->
                G = fun(X, {_, _, MaxPower} = Max) ->
                            case score(X, Y, Arr) of
                                N when N > MaxPower ->
                                    {X, Y, N};
                                _ ->
                                    Max
                            end
                    end,
                lists:foldl(G, Acc, lists:seq(1, ?N - ?D + 1))
        end,
    Init = {1, 1, score(1, 1, Arr)},
    {X, Y, _} = lists:foldl(F, Init, lists:seq(1, ?N - ?D + 1)),
    {X, Y}.

score(X, Y, Arr) ->
    lists:sum([array:get(I, array:get(J, Arr))
               || I <- lists:seq(X, X + ?D - 1), J <- lists:seq(Y, Y + ?D - 1)]).

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    X.
