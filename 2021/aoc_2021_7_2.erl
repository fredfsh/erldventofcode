-module(aoc_2021_7_2).

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
        {ok, [L]} ->
            lists:sort([list_to_integer(X) || X <- string:split(L, ",", all)])
    end.

ini() ->
    0.

do(L) ->
    Min = hd(L),
    Max = lists:last(L),
    hd(lists:sort([fuel(X, L) || X <- lists:seq(Min, Max)])).

fuel(X, L) ->
    lists:sum([abs(X - Y) * (abs(X - Y) + 1) div 2 || Y <- L]).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
