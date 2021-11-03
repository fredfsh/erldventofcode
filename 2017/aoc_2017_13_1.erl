-module(aoc_2017_13_1).

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
    case io:fread("", "~d: ~d") of
        eof ->
            eof;
        {ok, [Depth, Range]} ->
            {Depth, Range}
    end.

ini() ->
    0.

do({Depth, 1}) ->
    Depth;
do({Depth, Range}) ->
    Cycle = (Range - 1) * 2,
    case Depth rem Cycle of
        0 ->
            Depth * Range;
        _ ->
            0
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
