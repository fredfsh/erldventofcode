-module(aoc_2022_4_2).

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
    case io:fread("", "~d-~d,~d-~d") of
        eof ->
            eof;
        {ok, [A, B, C, D]} ->
            {A, B, C, D}
    end.

ini() ->
    0.

do({_A, B, C, _D}) when B < C ->
    0;
do({A, _B, _C, D}) when D < A ->
    0;
do(_) ->
    1.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
