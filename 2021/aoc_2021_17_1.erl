-module(aoc_2021_17_1).

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
    case io:fread("", "~a ~a x=~d..~d, y=~d..~d") of
        eof ->
            eof;
        {ok, [_, _, X1, X2, Y1, Y2]} ->
            {X1, X2, Y1, Y2}
    end.

ini() ->
    0.

-define(MAX_Y, 1000).

do({_, X2, Y1, _} = T) ->
    F = fun(X, Acc) ->
                G = fun(Y, Max) ->
                            max(Max, height(X, Y, T))
                    end,
                lists:foldl(G, Acc, lists:seq(Y1, ?MAX_Y))
        end,
    lists:foldl(F, 0, lists:seq(1, X2)).

height(X, Y, T) ->
    height_impl(0, 0, 0, 0, X, Y, T).

height_impl(Acc, _, X, Y, _, _, {_, X2, Y1, _}) when Y < Y1; X > X2 ->
    Acc;
height_impl(Acc, Highest, X, Y, DX, DY, {X1, X2, Y1, Y2} = T) ->
    NewAcc = case  X >= X1 andalso X =< X2 andalso Y >= Y1 andalso Y =< Y2 of
                 true ->
                     max(Acc, Highest);
                 false ->
                     Acc
             end,
    height_impl(NewAcc, max(Highest, Y), X + DX, Y + DY, max(DX-1, 0), DY-1, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
