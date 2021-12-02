-module(aoc_2021_2_1).

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
    {0, 0}.

do(X) ->
    X.

acc({X, Depth}, {forward, Y}) ->
    {X + Y, Depth};
acc({X, Depth}, {up, Y}) ->
    {X, max(0, Depth - Y)};
acc({X, Depth}, {down, Y}) ->
    {X, Depth + Y}.

fin({X, Y}) ->
    X * Y.
