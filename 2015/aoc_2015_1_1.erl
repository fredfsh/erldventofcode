-module(aoc_2015_1_1).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    do_impl(In, 0).

do_impl([], X) ->
    X;
do_impl([$( | T], X) ->
    do_impl(T, X + 1);
do_impl([$) | T], X) ->
    do_impl(T, X - 1).
