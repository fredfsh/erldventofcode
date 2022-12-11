-module(aoc_2022_10_1).

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
    case io:fread("", "~a") of
        eof ->
            eof;
        {ok, [noop]} ->
            noop;
        {ok, [addx]} ->
            {ok, [V]} = io:fread("", " ~d"),
            {addx, V}
    end.

-record(cpu, {x, cycle}).

ini() ->
    {#cpu{x = 1, cycle = 0}, 0}.

do(X) ->
    X.

acc({#cpu{x = X, cycle = Cycle} = CPU, Acc}, noop) ->
    {CPU#cpu{cycle = Cycle + 1}, Acc + X * check(Cycle + 1)};
acc({#cpu{x = X, cycle = Cycle}, Acc}, {addx, V}) ->
    {#cpu{x = X + V, cycle = Cycle + 2},
     Acc + X * check(Cycle + 1) + X * check(Cycle + 2)}.

check(Cycle)
  when Cycle >= 20 andalso Cycle =< 220 andalso (Cycle - 20) rem 40 =:= 0 ->
    Cycle;
check(_) ->
    0.

fin({_CPU, Signal}) ->
    Signal.
