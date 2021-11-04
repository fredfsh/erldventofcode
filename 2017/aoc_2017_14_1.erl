-module(aoc_2017_14_1).

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

-define(ROWS, 128).

do(X) ->
    lists:sum([do_impl(lists:append([X, "-", integer_to_list(R)]))
               || R <- lists:seq(0, ?ROWS - 1)]).

do_impl(X) ->
    Hash = aoc_2017_10_2:do(X),
    lists:sum([ones(dehex(C)) || C <- Hash]).

dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) ->
    C - $a + 10.

ones(X) ->
    ones_impl(0, X).

ones_impl(Acc, 0) ->
    Acc;
ones_impl(Acc, X) ->
    ones_impl(Acc + 1, X band (X - 1)).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
