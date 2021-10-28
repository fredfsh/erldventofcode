-module(aoc_2017_1_1).

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
    count([lists:last(X) | X]).

count(L) ->
    count_impl(0, L).

count_impl(Acc, [_]) ->
    Acc;
count_impl(Acc, [X, X | T]) ->
    count_impl(Acc + (X - $0), [X | T]);
count_impl(Acc, [_, X | T]) ->
    count_impl(Acc, [X | T]).
