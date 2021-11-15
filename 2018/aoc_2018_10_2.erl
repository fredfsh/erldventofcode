-module(aoc_2018_10_2).

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
    case io:fread("", "position=<~d,~d> velocity=<~d,~d>") of
        eof ->
            eof;
        {ok, [X, Y, VX, VY]} ->
            {X, Y, VX, VY}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    sim(0, X).

sim(Acc, L) ->
    case message(L) of
        true ->
            Acc;
        false ->
            sim(Acc + 1, move(L))
    end.

-define(HEIGHT, 10).  % experiement and find the right value

message(L) ->
    Sorted = lists:keysort(2, L),
    {_, MinY, _, _} = hd(Sorted),
    {_, MaxY, _, _} = lists:last(Sorted),
    MaxY - MinY < ?HEIGHT.

move(L) ->
    [{X + VX, Y + VY, VX, VY} || {X, Y, VX, VY} <- L].
