-module(aoc_2015_20_1).

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

do_impl(X, N) ->
    case sum(X) >= N of
        true ->
            X;
        _ ->
            do_impl(X + 1, N)
    end.

sum(X) ->
    sum_impl(10, 2, X).

sum_impl(Acc, _, 1) ->
    Acc;
sum_impl(Acc, Prime, X) when Prime * Prime > X ->
    Acc * (1 + X);
sum_impl(Acc, Prime, X) ->
    case X rem Prime of
        0 ->
            {SubSum, NewX} = sub_sum(X, Prime),
            sum_impl(Acc * SubSum, Prime + 1, NewX);
        _ ->
            sum_impl(Acc, Prime + 1, X)
    end.

sub_sum(X, Prime) ->
    sub_sum_impl(Prime, X, Prime).

sub_sum_impl(Acc, X, Prime) ->
    case X rem Prime of
        0 ->
            sub_sum_impl(Acc * Prime, X div Prime, Prime);
        _ ->
            {(Acc - 1) div (Prime - 1), X}
    end.
