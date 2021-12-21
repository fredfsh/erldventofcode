-module(aoc_2021_21_1).

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
    case io:fread("", "Player 1 starting position: ~d") of
        eof ->
            eof;
        {ok, [One]} ->
            {ok, [Two]} = io:fread("", "Player 2 starting position: ~d"),
            {One, Two}
    end.

ini() ->
    0.

do({One, Two}) ->
    sim(0, One, 0, Two, 0).

sim(Rolls, _, Score1, _, Score2) when Score1 >= 1000 ->
    Score2 * Rolls;
sim(Rolls, _, Score1, _, Score2) when Score2 >= 1000 ->
    Score1 * Rolls;
sim(Rolls, P1, Score1, P2, Score2) ->
    Move1 = (Rolls + 1) rem 100,
    Move2 = (Rolls + 2) rem 100,
    Move3 = (Rolls + 3) rem 100,
    NP = (P1 + Move1 + Move2 + Move3 - 1) rem 10 + 1,
    sim(Rolls + 3, P2, Score2, NP, Score1 + NP).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
