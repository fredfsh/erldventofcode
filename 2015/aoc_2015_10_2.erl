-module(aoc_2015_10_2).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    length(do_impl(In, 50)).

do_impl(X, 0) ->
    X;
do_impl(X, N) ->
    do_impl(look_and_say(X), N - 1).

look_and_say(X) ->
    look_and_say_impl(X, [], $0, 0).

look_and_say_impl([], Acc, X, C) ->
    lists:reverse([X, 16#30 + C | Acc]);
look_and_say_impl([X | T], [], _, 0) ->
    look_and_say_impl(T, [], X, 1);
look_and_say_impl([X | T], Acc, X, C) ->
    look_and_say_impl(T, Acc, X, C + 1);
look_and_say_impl([Y | T], Acc, X, C) ->
    look_and_say_impl(T, [X, 16#30 + C | Acc], Y, 1).
