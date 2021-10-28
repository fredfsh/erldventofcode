-module(aoc_2017_4_2).

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
            string:split(string:trim(L, trailing), " ", all)
    end.

do(X) ->
    L = [lists:sort(W) || W <- X],
    case length(lists:usort(L)) of
        N when N =:= length(L) ->
            1;
        _ ->
            0
    end.
