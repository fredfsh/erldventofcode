-module(aoc_2020_4_1).

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
    case io:get_line("") of
        eof ->
            Acc;
        "\n" ->
            Acc;
        X ->
            input_impl([X | Acc])
    end.

do(X) ->
    do_impl(X, sets:new()).

-define(REQUIRED, ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]).

do_impl([], Fields) ->
    case sets:is_subset(sets:from_list(?REQUIRED), Fields) of
        true ->
            1;
        _ ->
            0
    end;
do_impl([L | T], Fields) ->
    do_impl(T, add(L, Fields)).

add(L, Sets) ->
    add_impl(L, Sets, []).

-define(SPACE, 16#20).

add_impl([], Sets, _Acc) ->
    Sets;
add_impl([$: | T], Sets, Acc) ->
    add_impl(T, sets:add_element(lists:reverse(Acc), Sets), undefined);
add_impl([?SPACE | T], Sets, undefined) ->
    add_impl(T, Sets, []);
add_impl([X | T], Sets, Acc) when is_list(Acc) ->
    add_impl(T, Sets, [X | Acc]);
add_impl([_ | T], Sets, undefined) ->
    add_impl(T, Sets, undefined).
