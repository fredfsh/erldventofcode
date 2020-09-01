-module(aoc_2015_15_2).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl([]).

-define(FORMAT,
        "~s capacity ~d, durability ~d, flavor ~d, texture ~d, calories ~d").

input_impl(PL) ->
    case io:fread("", ?FORMAT) of
        eof ->
            PL;
        {ok, [_, C, D, F, T, Cal]} ->
            input_impl([{C, D, F, T, Cal} | PL])
    end.

-define(TOTAL, 100).

do(PL) ->
    do_impl(PL, ?TOTAL, {0, 0, 0, 0, 0}).

do_impl([Ingredient], Rem, Acc) ->
    product(Ingredient, Rem, Acc);
do_impl([Ingredient | T], Rem, Acc) ->
    loop(Ingredient, 0, 0, Rem, T, Acc).

loop(Ingredient, X, MaxAcc, Rem, T, Acc) ->
    Max = max(MaxAcc, do_impl(T, Rem - X, add(Ingredient, X, Acc))),
    case X of
        Rem ->
            Max;
        _ ->
            loop(Ingredient, X + 1, Max, Rem, T, Acc)
    end.

-define(TARGET, 500).

product(Ingredient, Vol, Acc) ->
    case add(Ingredient, Vol, Acc) of
        {C, D, F, T, ?TARGET} ->
            max(0, C) * max(0, D) * max(0, F) * max(0, T);
        _ ->
            0
    end.

add({C, D, F, T, Ca}, Vol, {CC, DD, FF, TT, Cal}) ->
    {CC + C * Vol, DD + D * Vol, FF + F * Vol, TT + T * Vol, Cal + Ca * Vol}.
