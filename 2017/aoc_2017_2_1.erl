-module(aoc_2017_2_1).

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
            input_numbers(L)
    end.

input_numbers(L) ->
    input_numbers_impl([], L).

input_numbers_impl(Acc, L) ->
    case string:to_integer(string:trim(L, leading)) of
        {error, no_integer} ->
            Acc;
        {X, T} ->
            input_numbers_impl([X | Acc], T)
    end.

do(X) ->
    lists:max(X) - lists:min(X).
