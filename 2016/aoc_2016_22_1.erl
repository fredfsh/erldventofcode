-module(aoc_2016_22_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    io:get_line(""),
    io:get_line(""),
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "/dev/grid/node-x~d-y~d ~dT ~dT ~dT ~d%") of
        eof ->
            Acc;
        {ok, [X, Y, Size, Used, Avail, UsePct]} ->
            input_impl([{{X, Y}, {Size, Used, Avail, UsePct}} | Acc])
    end.

do(X) ->
    length([1 || {A, {_, Used, _, _}} <- X,
                 {B, {_, _, Avail, _}} <- X,
                 Used =/= 0,
                 A =/= B,
                 Used =< Avail]).
