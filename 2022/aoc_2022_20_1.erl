-module(aoc_2022_20_1).

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
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [{X, false} | Acc].

fin(X) ->
    L = mix(lists:reverse(X)),
    %%io:format("~p~n", [L]),
    I = index(L),
    N = length(L),
    {K, _} = lists:nth((I + 1000) rem N + 1, L),
    {K2, _} = lists:nth((I + 2000) rem N + 1, L),
    {K3, _} = lists:nth((I + 3000) rem N + 1, L),
    K + K2 + K3.

mix(L) ->
    mix_impl(0, L).

mix_impl(N, L) when N =:= length(L) ->
    L;
mix_impl(N, [{_, true} = H | T]) ->
    mix_impl(N, lists:append(T, [H]));
mix_impl(N, [{X, false} | T] = L) ->
    %%io:format("~p~n", [L]),
    C = length(L) - 1,
    Move = (X rem C + C) rem C,
    {L1, L2} = lists:split(Move, T),
    mix_impl(N + 1, lists:append([L1, [{X, true}], L2])).

index(X) ->
    index_impl(0, X).

index_impl(Acc, [{0, true} | _]) ->
    Acc;
index_impl(Acc, [_ | T]) ->
    index_impl(Acc + 1, T).
