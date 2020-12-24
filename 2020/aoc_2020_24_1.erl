-module(aoc_2020_24_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

-define(TAB, tab).

run() ->
    ets:new(?TAB, [named_table]),
    run_impl().

run_impl() ->
    case io:get_line("") of
        eof ->
            F = fun({_, Black}, Acc) -> Acc + Black end,
            ets:foldl(F, 0, ?TAB);
        L ->
            do(string:trim(L, trailing)),
            run_impl()
    end.

do(L) ->
    do_impl(L, 0, 0).

do_impl([], X, Y) ->
    ets:update_counter(?TAB, {Y, X}, {2, 1, 1, 0}, {{Y, X}, 0});
do_impl([$e | T], X, Y) ->
    do_impl(T, X + 2, Y);
do_impl([$w | T], X, Y) ->
    do_impl(T, X - 2, Y);
do_impl([$s, $e | T], X, Y) ->
    do_impl(T, X + 1, Y - 1);
do_impl([$s, $w | T], X, Y) ->
    do_impl(T, X - 1, Y - 1);
do_impl([$n, $e | T], X, Y) ->
    do_impl(T, X + 1, Y + 1);
do_impl([$n, $w | T], X, Y) ->
    do_impl(T, X - 1, Y + 1).
