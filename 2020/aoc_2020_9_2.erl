-module(aoc_2020_9_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        [] ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "~d") of
        eof ->
            lists:reverse(Acc);
        {ok, [X]} ->
            input_impl([X | Acc])
    end.

-define(SUM, 1639024365).

do(X) ->
    ets:new(memo, [named_table]),
    L = init(X, 1, []),
    L2 = loop(X, 3, length(X), L),
    do_impl(?SUM, L2).

init([X | [Y | _] = T], I, L) ->
    init(T, I + 1, [{X + Y, {I, I + 1}, min(X, Y), max(X, Y)} | L]);
init(_, _, L) ->
    L.

loop([X | _], N, N, L) ->
    {Sum, Min, Max} = memo(2, N - 1, L),
    [{X + Sum, {1, N}, min(X, Min), max(X, Max)} | L];
loop(X, N, Length, L) ->
    L2 = loop_impl(X, N, Length, L, 1),
    loop(X, N + 1, Length, L2).

loop_impl(_X, N, Length, L, Start) when Start + N - 1 > Length ->
    L;
loop_impl(X, N, Length, L, Start) ->
    Now = lists:nth(Start, X),
    {Sum, Min, Max} = memo(Start + 1, Start + N - 1, L),
    L2 = [{Now + Sum, {Start, Start + N - 1}, min(Now, Min), max(Now, Max)} | L],
    loop_impl(X, N, Length, L2, Start + 1).


memo(A, B, L) ->
    {Sum, _, Min, Max} = lists:keyfind({A, B}, 2, L),
    {Sum, Min, Max}.

do_impl(X, L) ->
    {_, _, Min, Max} = lists:keyfind(X, 1, L),
    Min + Max.
