-module(aoc_2015_8_2).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(X) ->
    case io:fread("", "~s") of
        eof ->
            X;
        {ok, [S]} ->
            do_impl(X + count(S))
    end.

count(S) ->
    count_impl(S, 2).

count_impl([], X) ->
    X;
count_impl([$" | T], X) ->
    count_impl(T, X + 1);
count_impl([$\\ | T], X) ->
    count_impl(T, X + 1);
count_impl([_ | T], X) ->
    count_impl(T, X).
