-module(aoc_2024_13_1).

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

-define(FMT, "Button A: X+~d, Y+~d Button B: X+~d, Y+~d Prize: X=~d, Y=~d").

input() ->
    case io:fread("", ?FMT) of
        eof ->
            eof;
        {ok, [AX, AY, BX, BY, PX, PY]} ->
            {AX, AY, BX, BY, PX, PY}
    end.

ini() ->
    0.

do({AX, AY, BX, BY, PX, PY}) ->
    L = [{A, B} || A <- lists:seq(0, 100), B <- lists:seq(0, 100)],
    F = fun({A, B}) ->
                A * AX + B * BX =:= PX andalso A * AY + B * BY =:= PY
        end,
    case lists:filter(F, L) of
        [] ->
            0;
        [{A, B}] ->
            A * 3 + B
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
