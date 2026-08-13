-module(aoc_2025_1_2).

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
    {Hits, NewPos} = move(Pos, Dir, Moves),
    %%io:format("~w, ~w (~c~w) ~w, ~w~n", [Zeros, Pos, Dir, Moves, Zeros + Hits, NewPos]),
    {Zeros + Hits, NewPos}.

move(Pos, $L, Moves) ->
    NewPos = Pos - Moves,
    {hits(Pos, NewPos), NewPos rem 100};
move(Pos, $R, Moves) ->
    NewPos = Pos + Moves,
    {hits(Pos, NewPos), NewPos rem 100}.

hits(0, New) ->
    zeros(New) - 1;
hits(Old, 0) ->
    zeros(Old);
hits(Old, New) when Old * New < 0 ->
    zeros(Old) + zeros(New) - 1;
hits(Old, New) ->
    abs(zeros(New) - zeros(Old)).

zeros(X) when X < 0 ->
    zeros(-X);
zeros(X) ->
    X div 100 + 1.

fin({Zeros, _}) ->
    Zeros.
