-module(aoc_2017_17_1).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

do(X) ->
    do_impl(0, [0], 0, X).

-define(N, 2017).

do_impl(?N, L, Cur, _) ->
    lists:nth((Cur + 1) rem length(L) + 1, L);
do_impl(I, L, Cur, Steps) ->
    LI = (Cur + Steps) rem length(L) + 1,
    do_impl(I + 1, insert_after(LI, I + 1, L), LI, Steps).

insert_after(I, Val, L) ->
    {Left, Right} = lists:split(I, L),
    lists:append([Left, [Val], Right]).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
