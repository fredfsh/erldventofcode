-module(aoc_2020_15_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            L2 = string:trim(L, trailing),
            NStrs = string:split(L2, ",", all),
            F = fun(NStr, {I, Map, _Last}) ->
                        {N, _} = string:to_integer(NStr),
                        {I + 1, maps:put(N, I, Map), N}
                end,
            {_, Map, Last} = lists:foldl(F, {1, maps:new(), undefined}, NStrs),
            {Map, Last}
    end.

do({Map, Last}) ->
    S = maps:size(Map),
    do_impl(maps:remove(Last, Map), S + 1, Last).

-define(TURN, 30000000).

do_impl(_Map, N, Last) when N =:= ?TURN + 1 ->
    Last;
do_impl(Map, N, Last) ->
    Next = case maps:get(Last, Map, undefined) of
               undefined ->
                   0;
               X ->
                   N - 1 - X
           end,
    do_impl(maps:put(Last, N - 1, Map), N + 1, Next).
