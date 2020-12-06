-module(aoc_2020_6_1).

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

input_impl(L) ->
    case io:get_line("") of
        eof ->
            L;
        "\n" ->
            L;
        X ->
            input_impl([string:trim(X, trailing) | L])
    end.

do(X) ->
    do_impl(X, sets:new()).

do_impl([], Set) ->
    sets:size(Set);
do_impl([X | T], Set) ->
    do_impl(T, add(X, Set)).

add([], Set) ->
    Set;
add([X | T], Set) ->
    add(T, sets:add_element(X, Set)).
