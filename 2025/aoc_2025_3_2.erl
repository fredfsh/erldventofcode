-module(aoc_2025_3_2).

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

-define(N, 12).

do(X) ->
    Arr = array:new(?N + 1, {default, 0}),
    do_impl(Arr, integer_to_list(X)).

-define(a(I), array:get(I, Arr)).

do_impl(Arr, []) ->
    %%io:format("~w~n", [?a(?N)]),
    ?a(?N);
do_impl(Arr, [H | T]) ->
    F = fun(0, V) ->
                V;
           (I, V) ->
                max(?a(I - 1) * 10 + H - $0, V)
        end,
    do_impl(array:map(F, Arr), T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
