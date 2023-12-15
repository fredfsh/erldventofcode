-module(aoc_2023_15_1).

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
        {ok, [L]} ->
            string:split(string:trim(L), ",", all)
    end.

ini() ->
    0.

do(L) ->
    lists:sum([hash(S) || S <- L]).

hash(S) ->
    hash_impl(0, S).

hash_impl(Acc, []) ->
    Acc;
hash_impl(Acc, [H | T]) ->
    hash_impl((Acc + H) * 17 rem 256, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
