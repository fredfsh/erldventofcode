-module(aoc_2021_17_2).

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
    length([1 || X <- lists:seq(1,X2), Y <- lists:seq(Y1,?MAX_Y), hit(X, Y, T)]).


hit(X, Y, T) ->
    hit_impl(0, 0, X, Y, T).

hit_impl(X, Y, _, _, {_, X2, Y1, _}) when Y < Y1; X > X2 ->
    false;
hit_impl(X, Y, _, _, {X1, X2, Y1, Y2})
  when X >= X1 andalso X =< X2 andalso Y >= Y1 andalso Y =< Y2 ->
    true;
hit_impl(X, Y, DX, DY, T) ->
    hit_impl(X + DX, Y + DY, max(DX - 1, 0), DY - 1, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
