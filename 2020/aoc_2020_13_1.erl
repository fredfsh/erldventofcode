-module(aoc_2020_13_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            L = io:get_line(""),
            L2 = string:trim(L, trailing),
            Parts = string:split(L2, ",", all),
            F = fun("x", Acc) ->
                        Acc;
                   (NStr, Acc) ->
                        {Bus, _} = string:to_integer(NStr),
                        [Bus | Acc]
                end,
            {X, lists:foldl(F, [], Parts)}
    end.

do({X, Buses}) ->
    do_impl(X, Buses, undefined, undefined).

do_impl(_X, [], MinWait, MinBus) ->
    MinWait * MinBus;
do_impl(X, [Bus | _], _MinWait, _MinBus) when X rem Bus =:= 0 ->
    0;
do_impl(X, [Bus | T], MinWait, _MinBus) when Bus - X rem Bus < MinWait ->
    do_impl(X, T, Bus - X rem Bus, Bus);
do_impl(X, [_ | T], MinWait, MinBus) ->
    do_impl(X, T, MinWait, MinBus).
