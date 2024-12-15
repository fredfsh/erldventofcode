-module(aoc_2024_14_1).

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
    case io:fread("", "p=~d,~d v=~d,~d") of
        eof ->
            eof;
        {ok, [PX, PY, VX, VY]} ->
            {PX, PY, VX, VY}
    end.

ini() ->
    array:new([5, {default, 0}]).

do({PX, PY, VX, VY}) ->
    quadrant(PX, PY, VX, VY).

-define(COLS, 101).
-define(ROWS, 103).
-define(SECONDS, 100).

%%    0
%%  2 | 1
%% 0--+--0
%%  3 | 4
%%    0
quadrant(PX, PY, VX, VY) ->
    X = case (PX + VX * ?SECONDS) rem ?COLS of
            N when N < 0 ->
                N + ?COLS;
            N ->
                N
        end,
    Y = case (PY + VY * ?SECONDS) rem ?ROWS of
            M when M < 0 ->
                M + ?ROWS;
            M ->
                M
        end,
    quadrant_impl(X, Y).

quadrant_impl(X, _) when X =:= ?COLS div 2 ->
    0;
quadrant_impl(_, Y) when Y =:= ?ROWS div 2 ->
    0;
quadrant_impl(X, Y) when X > ?COLS div 2, Y > ?ROWS div 2 ->
    1;
quadrant_impl(X, Y) when X < ?COLS div 2, Y > ?ROWS div 2 ->
    2;
quadrant_impl(X, Y) when X < ?COLS div 2, Y < ?ROWS div 2 ->
    3;
quadrant_impl(X, Y) when X > ?COLS div 2, Y < ?ROWS div 2 ->
    4.

-define(a(I), array:get(I, Arr)).

acc(Arr, X) ->
    array:set(X, ?a(X) + 1, Arr).

fin(Arr) ->
    ?a(1) * ?a(2) * ?a(3) * ?a(4).
