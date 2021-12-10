-module(aoc_2021_10_1).

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
    corrupted([], X).

corrupted(_, []) ->
    0;
corrupted(L, [C | T]) when C =:= $(; C =:= $[; C =:= ${; C =:= $< ->
    corrupted([C | L], T);
corrupted([], [C | _]) ->
    score(C);
corrupted([H | T], [C | T2]) ->
    case pair(H, C) of
        true ->
            corrupted(T, T2);
        false ->
            score(C)
    end.

score($)) ->
    3;
score($]) ->
    57;
score($}) ->
    1197;
score($>) ->
    25137.

pair($(, $)) ->
    true;
pair($[, $]) ->
    true;
pair(${, $}) ->
    true;
pair($<, $>) ->
    true;
pair(_, _) ->
    false.


acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
