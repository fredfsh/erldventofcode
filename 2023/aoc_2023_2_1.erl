-module(aoc_2023_2_1).

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
    case io:fread("", "Game ~d: ") of
        eof ->
            eof;
        {ok, [I]} ->
            L = io:get_line(""),
            {I, string:trim(L)}
    end.

ini() ->
    0.

do({I, L}) ->
    case possible(L) of
        true ->
            I;
        false ->
            0
    end.

possible(L) ->
    Sets = string:split(L, ";", all),
    lists:all(fun set_possible/1, Sets).

-define(RGB, {12, 13, 14}).
set_possible(L) ->
    {R, G, B} = rgb(L),
    R =< element(1, ?RGB)
        andalso G =< element(2, ?RGB)
        andalso B =< element(3, ?RGB).

rgb(L) ->
    CubeStrs = string:split(L, ",", all),
    rgb_impl({0, 0, 0}, CubeStrs).

rgb_impl(RGB, []) ->
    RGB;
rgb_impl({R, G, B}, [H | T]) ->
    L = string:trim(H),
    {N, _} = string:to_integer(L),
    case {lists:suffix("red", L),
          lists:suffix("green", L),
          lists:suffix("blue", L)} of
        {true, false, false} ->
            rgb_impl({N, G, B}, T);
        {false, true, false} ->
            rgb_impl({R, N, B}, T);
        {false, false, true} ->
            rgb_impl({R, G, N}, T)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
