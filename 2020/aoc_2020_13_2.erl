-module(aoc_2020_13_2).

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
        {ok, [_]} ->
            L = io:get_line(""),
            L2 = string:trim(L, trailing),
            Parts = string:split(L2, ",", all),
            F = fun("x", {Acc, Plus}) ->
                        {Acc, Plus + 1};
                   (NStr, {Acc, Plus}) ->
                        {Bus, _} = string:to_integer(NStr),
                        {[{Plus, Bus} | Acc], Plus + 1}
                end,
            {Buses, _} = lists:foldl(F, {[], 0}, Parts),
            lists:reverse(Buses)
    end.

do(Buses) ->
    do_impl(Buses, 0, 1).

do_impl([], Time, _Mul) ->
    Time;
do_impl([{Plus, Bus} | T], Time, Mul) ->
    {T2, Mul2} = next(Plus, Bus, Time, Mul),
    do_impl(T, T2, Mul2).

next(Plus, Bus, Time, Mul) when (Time + Plus) rem Bus =:= 0 ->
    {Time, Mul * Bus};
next(Plus, Bus, Time, Mul) ->
    next(Plus, Bus, Time + Mul, Mul).
