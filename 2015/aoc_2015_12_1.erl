-module(aoc_2015_12_1).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    do_impl(In, 1, 0, 0).

do_impl([], _Sign, _Acc, Sum) ->
    Sum;
do_impl([$- | T], _Sign, Acc, Sum) ->
    do_impl(T, -1, Acc, Sum);
do_impl([X | T], Sign, Acc, Sum) when $0 =< X, X =< $9 ->
    do_impl(T, Sign, Acc * 10 + (X - $0), Sum);
do_impl([_ | T], Sign, Acc, Sum) ->
    do_impl(T, 1, 0, Sum + Acc * Sign).
