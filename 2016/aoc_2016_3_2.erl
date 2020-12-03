-module(aoc_2016_3_2).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(Acc) ->
    case io:fread("", "~d ~d ~d") of
        eof ->
            Acc;
        {ok, [A, B, C]} ->
            {ok, [D, E, F]} = io:fread("", "~d ~d ~d"),
            {ok, [G, H, I]} = io:fread("", "~d ~d ~d"),
            do_impl(Acc + count(A, D, G) + count(B, E, H) + count(C, F, I))
    end.

count(A, B, C) when A + B > C, A + C > B, B + C > A -> 1;
count(_, _, _) -> 0.
