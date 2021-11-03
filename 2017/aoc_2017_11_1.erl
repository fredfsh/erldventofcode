-module(aoc_2017_11_1).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            string:split(X, ",", all)
    end.

ini() ->
    0.

do(X) ->
    do_impl(X, 0, 0, 0).

do_impl([], X, Y, Acc) ->
    max(Acc, dist(X, Y));
do_impl(["n" | T], X, Y, Acc) ->
    do_impl(T, X, Y + 2, max(Acc, dist(X, Y)));
do_impl(["ne" | T], X, Y, Acc) ->
    do_impl(T, X + 1, Y + 1, max(Acc, dist(X, Y)));
do_impl(["se" | T], X, Y, Acc) ->
    do_impl(T, X + 1, Y - 1, max(Acc, dist(X, Y)));
do_impl(["s" | T], X, Y, Acc) ->
    do_impl(T, X, Y - 2, max(Acc, dist(X, Y)));
do_impl(["sw" | T], X, Y, Acc) ->
    do_impl(T, X - 1, Y - 1, max(Acc, dist(X, Y)));
do_impl(["nw" | T], X, Y, Acc) ->
    do_impl(T, X - 1, Y + 1, max(Acc, dist(X, Y))).

dist(X, Y) ->
    (abs(X) + abs(Y)) div 2.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
