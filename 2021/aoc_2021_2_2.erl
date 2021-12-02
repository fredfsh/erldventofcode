-module(aoc_2021_2_2).

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
    case io:fread("", "~a ~d") of
        eof ->
            eof;
        {ok, [Action, X]} ->
            {Action, X}
    end.

ini() ->
    {0, 0, 0}.

do(X) ->
    X.

acc({X, Depth, Aim}, {forward, Y}) ->
    {X + Y, Depth + Aim * Y, Aim};
acc({X, Depth, Aim}, {up, Y}) ->
    {X, Depth, Aim - Y};
acc({X, Depth, Aim}, {down, Y}) ->
    {X, Depth, Aim + Y}.

fin({X, Y, _}) ->
    X * Y.
