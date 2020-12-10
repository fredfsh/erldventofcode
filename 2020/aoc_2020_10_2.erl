-module(aoc_2020_10_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        [] ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl([]).

input_impl(L) ->
    case io:fread("", "~d") of
        eof ->
            L;
        {ok, [X]} ->
            input_impl([X | L])
    end.

do(L) ->
    L2 = lists:sort(L),
    do_impl(L2, {0, 1}, undefined, undefined).

do_impl([], {_L1, C1}, {_L2, _C2}, {_L3, _C3}) ->
    C1;
do_impl([X | T], {L1, C1}, G2, G3) ->
    Acc = 0,
    Acc2 = case X - L1 =< 3 of
               true ->
                   Acc + C1;
               _ ->
                   Acc
           end,
    Acc3 = case G2 of
               {L2, C2} when X - L2 =< 3 ->
                   Acc2 + C2;
               _ ->
                   Acc2
           end,
    Acc4 = case G3 of
               {L3, C3} when X - L3 =< 3 ->
                   Acc3 + C3;
               _ ->
                   Acc3
           end,
    do_impl(T, {X, Acc4}, {L1, C1}, G2).
