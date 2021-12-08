-module(aoc_2021_8_1).

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
    case io:get_line("") of
        eof ->
            eof;
        L ->
            [Left, Right] = string:split(string:trim(L, trailing), " | "),
            {string:split(Left, " ", all), string:split(Right, " ", all)}

    end.

ini() ->
    0.

do({_, L}) ->
    length(lists:filter(fun(X) -> lists:member(length(X), [2, 4, 3, 7]) end, L)).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
