-module(aoc_2021_20_2).

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
    {ok, [S]} = io:fread("", "~s"),
    io:get_line(""),
    {array:from_list(S), maps:new(), 0, undefined}.

do(X) ->
    X.

acc({Arr, Lights, Y, _}, L) ->
    F = fun(C, {X, Acc}) ->
                {X + 1, maps:put({X, Y}, C, Acc)}
        end,
    {_, NewLights} = lists:foldl(F, {0, Lights}, L),
    {Arr, NewLights, Y + 1, length(L)}.

-define(N, 50).

fin({Arr, Lights, Rows, Cols}) ->
    F = fun(I, {Acc, Default}) ->
                Min = -2 * I,
                Max = 2 * I - 1,
                {tr(Acc, Min, Cols + Max, Min, Rows + Max, Arr, Default),
                 default(Default, Arr)}
        end,
    {NewLights, _} = lists:foldl(F, {Lights, $.}, lists:seq(1, ?N)),
    count(NewLights).

tr(Lights, MinX, MaxX, MinY, MaxY, Arr, Default) ->
    L = [{{X, Y}, next(X, Y, Lights, Arr, Default)}
         ||  X <- lists:seq(MinX, MaxX), Y <- lists:seq(MinY, MaxY)],
    maps:from_list(L).

next(X, Y, Lights, Arr, Default) ->
    Seq = [-1, 0, 1],
    S = [case maps:get({X + DX, Y + DY}, Lights, Default) of
             $# ->
                 $1;
             $. ->
                 $0
         end || DY <- Seq, DX <- Seq],
    array:get(list_to_integer(S, 2), Arr).

default($., Arr) ->
    array:get(0, Arr);
default($#, Arr) ->
    array:get(array:size(Arr) - 1, Arr).

count(Lights) ->
    F = fun(_, $#, Acc) ->
                Acc + 1;
           (_, $., Acc) ->
                Acc
        end,
    maps:fold(F, 0, Lights).
