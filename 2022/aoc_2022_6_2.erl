-module(aoc_2022_6_2).

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
    do_impl(0, X).

-define(N, 14).

do_impl(I, [_ | T] = L) ->
    {First, _} = lists:split(?N, L),
    case diff(First) of
        true ->
            I + ?N;
        false ->
            do_impl(I + 1, T)
    end.

diff(L) ->
    sets:size(sets:from_list(L)) =:= ?N.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
