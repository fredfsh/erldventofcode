-module(aoc_2020_2_2).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(Acc) ->
    case io:fread("", "~d-~d ~c: ~s") of
        eof ->
            Acc;
        {ok, [Low, High, [Char], Str]} ->
            case valid(Low, High, Char, Str) of
                true ->
                    do_impl(Acc + 1);
                _ ->
                    do_impl(Acc)
            end
    end.

valid(L, H, C, S) ->
    (lists:nth(L, S) =:= C andalso lists:nth(H, S) =/= C) orelse
    (lists:nth(L, S) =/= C andalso lists:nth(H, S) =:= C).
