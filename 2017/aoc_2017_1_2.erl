-module(aoc_2017_1_2).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    {L1, L2} = lists:split(length(X) div 2, X),
    count(L1, L2).

count(L1, L2) ->
    count_impl(0, L1, L2).

count_impl(Acc, [], []) ->
    Acc * 2;
count_impl(Acc, [X | T1], [X | T2]) ->
    count_impl(Acc + (X - $0), T1, T2);
count_impl(Acc, [_ | T1], [_ | T2]) ->
    count_impl(Acc, T1, T2).
