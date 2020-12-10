-module(aoc_2020_9_1).

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

input_impl(Acc) ->
    case io:fread("", "~d") of
        eof ->
            lists:reverse(Acc);
        {ok, [X]} ->
            input_impl([X | Acc])
    end.

-define(PREAMBLE, 25).

do(X) ->
    do_impl(X, []).

do_impl([X | T], L) when length(L) < ?PREAMBLE ->
    do_impl(T, [X | L]);
do_impl([X | T], L) ->
    case valid(X, L) of
        true ->
            do_impl(T, [X | lists:droplast(L)]);
        _ ->
            X
    end.

valid(X, L) ->
    F = fun(Element) ->
                lists:member(X - Element, lists:keydelete(Element, 1, L))
        end,
    lists:any(F, L).
