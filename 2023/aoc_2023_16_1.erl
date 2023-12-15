-module(aoc_2023_16_1).

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
            array:from_list(string:trim(X))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(UPWARDS, upwards).
-define(DOWNWARDS, downwards).
-define(LEFTWARDS, leftwards).
-define(RIGHTWARDS, rightwards).

fin(Mirrors) ->
    ets:new(?UPWARDS, [named_table]),
    ets:new(?DOWNWARDS, [named_table]),
    ets:new(?LEFTWARDS, [named_table]),
    ets:new(?RIGHTWARDS, [named_table]),
    trace(0, 0, 1, 0, Mirrors),
    energized().

trace(X, Y, DX, DY, Mirrors) ->
    Tab = dir(DX, DY),
    case ets:lookup(Tab, {X, Y}) of
        [] ->
            ets:insert_new(Tab, {{X, Y}, true}),
            Nexts = nexts(X, Y, DX, DY, Mirrors),
            F = fun({NX, NY, NDX, NDY}) ->
                        trace(NX, NY, NDX, NDY, Mirrors)
                end,
            lists:foreach(F, Nexts);
        _ ->
            ok
    end.

dir( 0, -1) -> ?UPWARDS;
dir( 0,  1) -> ?DOWNWARDS;
dir(-1,  0) -> ?LEFTWARDS;
dir( 1,  0) -> ?RIGHTWARDS.

-define(a(X, Y), array:get(X, array:get(Y, Mirrors))).

nexts(X, Y, DX, DY, Mirrors) ->
    L = case {?a(X, Y), dir(DX, DY)} of
            {$., _} ->
                [{X + DX, Y + DY, DX, DY}];
            {$/, ?UPWARDS} ->
                [{X + 1, Y, 1, 0}];
            {$/, ?DOWNWARDS} ->
                [{X - 1, Y, -1, 0}];
            {$/, ?LEFTWARDS} ->
                [{X, Y + 1, 0, 1}];
            {$/, ?RIGHTWARDS} ->
                [{X, Y - 1, 0, -1}];
            {$\\, ?UPWARDS} ->
                [{X - 1, Y, -1, 0}];
            {$\\, ?DOWNWARDS} ->
                [{X + 1, Y, 1, 0}];
            {$\\, ?LEFTWARDS} ->
                [{X, Y - 1, 0, -1}];
            {$\\, ?RIGHTWARDS} ->
                [{X, Y + 1, 0, 1}];
            {$|, ?UPWARDS} ->
                [{X, Y - 1, 0, -1}];
            {$|, ?DOWNWARDS} ->
                [{X, Y + 1, 0, 1}];
            {$|, ?LEFTWARDS} ->
                [{X, Y - 1, 0, -1}, {X, Y + 1, 0, 1}];
            {$|, ?RIGHTWARDS} ->
                [{X, Y - 1, 0, -1}, {X, Y + 1, 0, 1}];
            {$-, ?UPWARDS} ->
                [{X - 1, Y, -1, 0}, {X + 1, Y, 1, 0}];
            {$-, ?DOWNWARDS} ->
                [{X - 1, Y, -1, 0}, {X + 1, Y, 1, 0}];
            {$-, ?LEFTWARDS} ->
                [{X - 1, Y, -1, 0}];
            {$-, ?RIGHTWARDS} ->
                [{X + 1, Y, 1, 0}]
        end,
    Rows = array:size(Mirrors),
    Cols = array:size(array:get(0, Mirrors)),
    F = fun({FX, FY, _DX, _DY}) ->
                FX >= 0 andalso FX < Cols andalso FY >= 0 andalso FY < Rows
        end,
    lists:filter(F, L).

energized() ->
    F = fun(Tab) ->
                sets:from_list(proplists:get_keys(ets:tab2list(Tab)))
        end,
    L = lists:map(F, [?UPWARDS, ?DOWNWARDS, ?LEFTWARDS, ?RIGHTWARDS]),
    sets:size(sets:union(L)).
