-module(aoc_2016_20_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "~d-~d") of
        eof ->
            Acc;
        {ok, [L, R]} ->
            input_impl([{L, R} | Acc])
    end.

do(X) ->
    Firewall = lists:usort(X),
    count(Firewall).

count(Firewall) ->
    count_impl(0, 0, Firewall).

-define(MAX_INT, 4294967295).

count_impl(Count, X, _) when X > ?MAX_INT ->
    Count;
count_impl(Count, X, []) ->
    Count + (?MAX_INT - X + 1);
count_impl(Count, X, [{L, R} | T]) ->
    count_impl(Count + max(0, L - X), max(R + 1, X), T).
