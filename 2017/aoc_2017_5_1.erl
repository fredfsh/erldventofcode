-module(aoc_2017_5_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(0, maps:new()).

input_impl(I, Map) ->
    case io:fread("", "~d") of
        eof ->
            Map;
        {ok, [X]} ->
            input_impl(I + 1, maps:put(I, X, Map))
    end.

do(X) ->
    run(0, 0, X).

run(Acc, IP, Code) ->
    case maps:get(IP, Code, undefined) of
        undefined ->
            Acc;
        Jmp ->
            run(Acc + 1, IP + Jmp, maps:put(IP, Jmp + 1, Code))
    end.
