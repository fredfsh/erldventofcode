-module(aoc_2023_1_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(ini()).

run_impl(Acc) ->
    case input() of
        eof ->
            fin(Acc);
        X ->
            run_impl(acc(Acc, do(X)))
    end.

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

do(X) ->
    do_impl(undefined, undefined, X).

do_impl(First, Last, []) ->
    First * 10 + Last;
do_impl(First, Last, [_ | T] = L) ->
    case digit(L) of
        undefined ->
            do_impl(First, Last, T);
        X when First =:= undefined ->
            do_impl(X, X, T);
        X ->
            do_impl(First, X, T)
    end.

-define(DIGITS, lists:zip(lists:seq(1, 9), ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])).

digit([H | _]) when H >= $0, H =< $9 ->
    H - $0;
digit(L) ->
    F = fun({_, Spell}) -> not lists:prefix(Spell, L) end,
    case lists:dropwhile(F, ?DIGITS) of
        [] ->
            undefined;
        [{I, _} | _] ->
            I
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
