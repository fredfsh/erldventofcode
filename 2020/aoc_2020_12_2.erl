-module(aoc_2020_12_2).

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

input_impl(L) ->
    case io:fread("", "~c~d") of
        eof ->
            lists:reverse(L);
        {ok, [[A], N]} ->
            input_impl([{A, N} | L])
    end.

do(X) ->
    do_impl(X, 0, 0, 10, 1).

do_impl([], X, Y, _, _) ->
    abs(X) + abs(Y);
do_impl([{A, N} | T], X, Y, DX, DY) ->
    {X2, Y2, DX2, DY2} = move(A, N, X, Y, DX, DY),
    do_impl(T, X2, Y2, DX2, DY2).

move($N, N, X, Y, DX, DY) ->
    {X, Y, DX, DY + N};
move($S, N, X, Y, DX, DY) ->
    {X, Y, DX, DY - N};
move($E, N, X, Y, DX, DY) ->
    {X, Y, DX + N, DY};
move($W, N, X, Y, DX, DY) ->
    {X, Y, DX - N, DY};
move($F, N, X, Y, DX, DY) ->
    {X + DX * N, Y + DY * N, DX, DY};
move($L, N, X, Y, DX, DY) ->
    move($R, 360 - N, X, Y, DX, DY);
move($R, N, X, Y, DX, DY) ->
    {DX2, DY2} = turn_right(N, DX, DY),
    {X, Y, DX2, DY2}.

turn_right(N, DX, DY) when N >= 360 ->
    turn_right(N rem 360, DX, DY);
turn_right(0, DX, DY) ->
    {DX, DY};
turn_right(90, DX, DY) ->
    {DY, -DX};
turn_right(180, DX, DY) ->
    {-DX, -DY};
turn_right(270, DX, DY) ->
    {-DY, DX}.
