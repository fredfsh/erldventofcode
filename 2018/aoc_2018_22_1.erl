-module(aoc_2018_22_1).

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
    case io:fread("", "depth: ~d target: ~d,~d") of
        eof ->
            eof;
        {ok, [Depth, X, Y]} ->
            {Depth, X, Y}
    end.

ini() ->
    0.

-define(EROSION, erosion).

do({Depth, TX, TY}) ->
    ets:new(?EROSION, [named_table]),
    lists:sum([risk(X, Y, Depth, TX, TY) || X <- lists:seq(0, TX),
                                            Y <- lists:seq(0, TY)]).

risk(X, Y, Depth, TX, TY) ->
    erosion(X, Y, Depth, TX, TY) rem 3.

erosion(X, Y, Depth, TX, TY) ->
    case ets:lookup(?EROSION, {X, Y}) of
        [] ->
            Erosion = (geology(X, Y, Depth, TX, TY) + Depth) rem 20183,
            ets:insert_new(?EROSION, {{X, Y}, Erosion}),
            Erosion;
        [{_, Erosion}] ->
            Erosion
    end.

geology(0, 0, _, _, _) ->
    0;
geology(TX, TY, _, TX, TY) ->
    0;
geology(X, 0, _, _, _) ->
    X * 16807;
geology(0, Y, _, _, _) ->
    Y * 48271;
geology(X, Y, Depth, TX, TY) ->
    erosion(X - 1, Y, Depth, TX, TY) * erosion(X, Y - 1, Depth, TX, TY).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
