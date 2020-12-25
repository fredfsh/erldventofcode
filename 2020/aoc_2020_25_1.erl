-module(aoc_2020_25_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    {ok, [CPub]} = io:fread("", "~d"),
    {ok, [DPub]} = io:fread("", "~d"),
    do(CPub, DPub).

-define(MOD, 20201227).

do(CPub, DPub) ->
    do_impl(CPub, DPub, 1, 7, 0).

do_impl(CPub, DPub, CPub, _Subject, Loop) ->
    transform(1, DPub, Loop);
do_impl(CPub, DPub, DPub, _Subject, Loop) ->
    transform(1, CPub, Loop);
do_impl(CPub, DPub, Val, Subject, Loop) ->
    do_impl(CPub, DPub, transform_impl(Val, Subject), Subject, Loop + 1).

transform_impl(Val, Subject) ->
    Val * Subject rem ?MOD.

transform(Val, _Subject, 0) ->
    Val;
transform(Val, Subject, N) ->
    transform(transform_impl(Val, Subject), Subject, N - 1).
