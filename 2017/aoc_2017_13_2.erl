-module(aoc_2017_13_2).

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
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    enter(0, X, X).

enter(Acc, [], _) ->
    Acc;
enter(Time, [{Depth, Range} | T], Layers) ->
    case (Time + Depth) rem (2 * (Range - 1)) of
        0 ->
            enter(Time + 1, Layers, Layers);
        _ ->
            enter(Time, T, Layers)
    end.
