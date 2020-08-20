-module(aoc_2015_1_2).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    do_impl(In, 0, 0).

do_impl([$) | _], 0, N) ->
    N + 1;
do_impl([$( | T], X, N) ->
    do_impl(T, X + 1, N + 1);
do_impl([$) | T], X, N) ->
    do_impl(T, X - 1, N + 1).
