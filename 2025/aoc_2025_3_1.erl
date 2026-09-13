-module(aoc_2025_3_1).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

do(X) ->
    S = integer_to_list(X),
    Prefix = lists:droplast(S),
    First = lists:max(Prefix) - $0,
    Index = index(First, S),
    {_, Suffix} = lists:split(Index, S),
    Second = lists:max(Suffix) - $0,
    First * 10 + Second.

index(X, L) ->
    index_impl(1, X + $0, L).

index_impl(Res, X, [X | _]) ->
    Res;
index_impl(Res, X, [_ | T]) ->
    index_impl(Res + 1, X, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
