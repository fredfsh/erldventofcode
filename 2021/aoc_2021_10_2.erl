-module(aoc_2021_10_2).

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
    [].

do(X) ->
    incomplete([], X).

incomplete(L, []) ->
    score(L);
incomplete(L, [C | T]) when C =:= $(; C =:= $[; C =:= ${; C =:= $< ->
    incomplete([C | L], T);
incomplete([], _) ->
    undefined;
incomplete([H | T], [C | T2]) ->
    case pair(H, C) of
        true ->
            incomplete(T, T2);
        false ->
            undefined
    end.

score(L) when is_list(L) ->
    score_impl(0, L);
score($() ->
    1;
score($[) ->
    2;
score(${) ->
    3;
score($<) ->
    4.

score_impl(Acc, []) ->
    Acc;
score_impl(Acc, [H | T]) ->
    score_impl(Acc * 5 + score(H), T).

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

acc(Acc, undefined) ->
    Acc;
acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    N = length(X),
    L = lists:sort(X),
    lists:nth((N + 1) div 2, L).
