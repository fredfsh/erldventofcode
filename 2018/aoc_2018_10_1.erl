-module(aoc_2018_10_1).

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
    case io:fread("", "position=<~d,~d> velocity=<~d,~d>") of
        eof ->
            eof;
        {ok, [X, Y, VX, VY]} ->
            {X, Y, VX, VY}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    sim(X).

sim(L) ->
    case message(L) of
        true ->
            print(L);
        false ->
            sim(move(L))
    end.

-define(HEIGHT, 10).  % experiement and find the right value

message(L) ->
    Sorted = lists:keysort(2, L),
    {_, MinY, _, _} = hd(Sorted),
    {_, MaxY, _, _} = lists:last(Sorted),
    MaxY - MinY < ?HEIGHT.

print([{X0, Y0, _, _} | T]) ->
    Init = sets:from_list([{X0, Y0}]),
    F = fun({X, Y, _, _}, {SetAcc, MinX, MinY, MaxX, MaxY}) ->
                {sets:add_element({X, Y}, SetAcc),
                 min(X, MinX),
                 min(Y, MinY),
                 max(X, MaxX),
                 max(Y, MaxY)}
        end,
    {Set, MinX, MinY, MaxX, MaxY} = lists:foldl(F, {Init, X0, Y0, X0, Y0}, T),
    FY = fun(Y) ->
                 Line = [case sets:is_element({X, Y}, Set) of
                             true ->
                                 $#;
                             false ->
                                 $.
                         end || X <- lists:seq(MinX, MaxX)],
                 io:format("~p~n", [Line])
         end,
    lists:foreach(FY, lists:seq(MinY, MaxY)).

move(L) ->
    [{X + VX, Y + VY, VX, VY} || {X, Y, VX, VY} <- L].
