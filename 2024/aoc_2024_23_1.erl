-module(aoc_2024_23_1).

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
            [A, B] = string:lexemes(X, "-"),
            {A, B}
    end.

ini() ->
    sets:new().

do(X) ->
    X.

acc(Acc, {A, B}) ->
    sets:add_element({A, B}, sets:add_element({B, A}, Acc)).

-define(X(A, B), sets:is_element({A, B}, Links)).

fin(Links) ->
    Coms = computers(Links),
    Ts = [C || C <- Coms, hd(C) =:= $t],
    Unsorted = [[T, A, B] || T <- Ts, A <- Coms, B <- Coms, ?X(A, B), ?X(A, T), ?X(B, T)],
    Sorted = [lists:sort(X) || X <- Unsorted],
    sets:size(sets:from_list(Sorted)).

computers(Links) ->
    L = [[A, B] || {A, B} <- sets:to_list(Links)],
    sets:to_list(sets:from_list(lists:append(L))).
