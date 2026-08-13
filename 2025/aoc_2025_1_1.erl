-module(aoc_2025_1_1).

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
    case io:fread("", "~c~d") of
        eof ->
            eof;
        {ok, [[D], X]} ->
            {D, X}
    end.

ini() ->
    {0, 50}.

do(X) ->
    X.

acc({Zeros, Pos}, {Dir, Moves}) ->
    acc_impl(Zeros, Pos, Dir, Moves).

acc_impl(Zeros, Pos, Dir, Moves) ->
    case move(Pos, Dir, Moves) of
        0 ->
            {Zeros + 1, 0};
        NewPos ->
            {Zeros, NewPos}
    end.

move(Pos, $L, Moves) ->
    (Pos + 100 - Moves) rem 100;
move(Pos, $R, Moves) ->
    (Pos + Moves) rem 100.

fin({Zeros, _}) ->
    Zeros.
