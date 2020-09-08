-module(aoc_2015_20_2).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    {ok, [In]} = io:fread("", "~d"),
    In.

do(X) ->
    do_impl(1, X).

do_impl(N, X) ->
    case sum(N) >= X of
        true ->
            N;
        _ ->
            do_impl(N + 1, X)
    end.

sum(X) ->
    sum_impl(0, 1, X).

-define(UNIT, 11).
-define(MAX, 50).

sum_impl(Acc, N, _X) when N > ?MAX ->
    Acc;
sum_impl(Acc, N, X) when X rem N =:= 0 ->
    sum_impl(Acc + X div N * ?UNIT, N + 1, X);
sum_impl(Acc, N, X) ->
    sum_impl(Acc, N + 1, X).
