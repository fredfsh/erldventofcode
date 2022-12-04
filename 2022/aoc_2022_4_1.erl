-module(aoc_2022_4_1).

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

do({A, B, C, D}) when A =< C andalso B >= D ->
    1;
do({A, B, C, D}) when A >= C andalso B =< D ->
    1;
do(_) ->
    0.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
