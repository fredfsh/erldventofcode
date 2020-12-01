-module(aoc_2020_1_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(sets:new()).

do_impl(Set) ->
    {ok, [N]} = io:fread("", "~d"),
    case sets:is_element(2020 - N, Set) of
        true ->
            (2020 - N) * N;
        _ ->
            do_impl(sets:add_element(N, Set))
    end.
