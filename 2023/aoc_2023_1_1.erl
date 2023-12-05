-module(aoc_2023_1_1).

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
            X
    end.

ini() ->
    0.

do(X) ->
    do_impl(undefined, undefined, X).

do_impl(First, Last, []) ->
    First * 10 + Last;
do_impl(undefined, undefined, [H | T]) when H >= $0, H =< $9 ->
    X = H - $0,
    do_impl(X, X, T);
do_impl(First, _, [H | T]) when H >= $0, H =< $9 ->
    do_impl(First, H - $0, T);
do_impl(First, Last, [_ | T]) ->
    do_impl(First, Last, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
