-module(aoc_2016_20_1).

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
    lowest(Firewall).

lowest(Firewall) ->
    lowest_impl(0, Firewall).

lowest_impl(X, [{L, _R} | _T]) when X < L ->
    X;
lowest_impl(X, [{_L, R} | T]) ->
    lowest_impl(max(R + 1, X), T).
