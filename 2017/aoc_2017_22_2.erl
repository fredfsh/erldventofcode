-module(aoc_2017_22_2).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    {maps:new(), 0, undefined}.

do(X) ->
    X.

acc({Map, Y, _}, L) ->
    F = fun($#, {Acc, X}) ->
                {maps:put({X, Y}, infected, Acc), X + 1};
           ($., {Acc, X}) ->
                {Acc, X + 1}
        end,
    {NewMap, _} = lists:foldl(F, {Map, 0}, L),
    {NewMap, Y + 1, length(L)}.

fin({Map, _, N}) ->
    burst(0, 0, (N - 1) div 2, (N - 1) div 2, 0, -1, Map).

-define(N, 10000000).

burst(Acc, ?N, _, _, _, _, _) ->
    Acc;
burst(Acc, I, X, Y, DX, DY, Map) ->
    State = maps:get({X, Y}, Map, clean),
    {NDX, NDY} = dir(DX, DY, State),
    NewMap = maps:put({X, Y}, state(State), Map),
    NewAcc = case State of weakened -> Acc + 1; _ -> Acc end,
    burst(NewAcc, I + 1, X + NDX, Y + NDY, NDX, NDY, NewMap).

dir(DX, DY, clean) ->
    turn_left(DX, DY);
dir(DX, DY, weakened) ->
    {DX, DY};
dir(DX, DY, infected) ->
    turn_right(DX, DY);
dir(DX, DY, flagged) ->
    {-DX, -DY}.

turn_left(0, 1) -> {1, 0};
turn_left(1, 0) -> {0, -1};
turn_left(0, -1) -> {-1, 0};
turn_left(-1, 0) -> {0, 1}.

turn_right(0, 1) -> {-1, 0};
turn_right(-1, 0) -> {0, -1};
turn_right(0, -1) -> {1, 0};
turn_right(1, 0) -> {0, 1}.

state(clean) -> weakened;
state(weakened) -> infected;
state(infected) -> flagged;
state(flagged) -> clean.
