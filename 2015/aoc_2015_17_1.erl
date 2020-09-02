-module(aoc_2015_17_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl([]).

input_impl(L) ->
    case io:fread("", "~d") of
        eof ->
            L;
        {ok, [C]} ->
            input_impl([C | L])
    end.

-define(VOL, 150).
-define(TAB, memo).

do(L) ->
    ets:new(?TAB, [named_table]),
    memo(?VOL, L).

memo(Vol, L) ->
    N = length(L),
    Key = {Vol, N},
    case ets:lookup(?TAB, Key) of
        [{_, X}] ->
            X;
        [] ->
            Y = memo_impl(Vol, L),
            ets:insert(?TAB, {Key, Y}),
            Y
    end.

memo_impl(0, _) ->
    1;
memo_impl(_, []) ->
    0;
memo_impl(X, [Y | T]) when Y > X ->
    memo(X, T);
memo_impl(X, [Y | T]) ->
    memo(X, T) + memo(X - Y, T).
