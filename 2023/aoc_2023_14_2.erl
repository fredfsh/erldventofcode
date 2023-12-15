-module(aoc_2023_14_2).

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
        {ok, [L]} ->
            string:trim(L)
    end.

ini() ->
    {sets:new(), sets:new(), 0, undefined}.

do(X) ->
    X.

acc({Balls, Blocks, Rows, _}, L) ->
    NewRows = Rows + 1,
    F = fun($., {BallsAcc, BlocksAcc, NextColAcc}) ->
                {BallsAcc, BlocksAcc, NextColAcc + 1};
           ($O, {BallsAcc, BlocksAcc, NextColAcc}) ->
                {sets:add_element({NewRows, NextColAcc}, BallsAcc),
                 BlocksAcc,
                 NextColAcc + 1};
           ($#, {BallsAcc, BlocksAcc, NextColAcc}) ->
                {BallsAcc,
                 sets:add_element({NewRows, NextColAcc}, BlocksAcc),
                 NextColAcc + 1}
        end,
    {NewBalls, NewBlocks, _} = lists:foldl(F, {Balls, Blocks, 1}, L),
    {NewBalls, NewBlocks, NewRows, length(L)}.

-define(TAB_LOAD, loads).
-define(TAB_INDEX, index).
-define(CYCLES, 1000000000).

fin({Balls, Blocks, Rows, Cols}) ->
    ets:new(?TAB_LOAD, [named_table]),
    ets:new(?TAB_INDEX, [named_table]),
    tilt(1, Balls, Blocks, Rows, Cols).

tilt(Cycle, Balls, Blocks, Rows, Cols) ->
    {BallsN, LoadN} = tilt_north(Balls, Blocks, Rows, Cols),
    {BallsW, LoadW} = tilt_west(BallsN, Blocks, Rows, Cols),
    {BallsS, LoadS} = tilt_south(BallsW, Blocks, Rows, Cols),
    {BallsE, LoadE} = tilt_east(BallsS, Blocks, Rows, Cols),
    Loads = {LoadN, LoadW, LoadS, LoadE},
    case ets:lookup(?TAB_INDEX, Loads) of
        [] ->
            ets:insert_new(?TAB_INDEX, {Loads, Cycle}),
            ets:insert_new(?TAB_LOAD, {Cycle, Loads}),
            tilt(Cycle + 1, BallsE, Blocks, Rows, Cols);
        [{_, C}] ->
            Period = Cycle - C,
            Index = (?CYCLES - C) rem Period + C,
            {_, _, _, Res} = ets:lookup_element(?TAB_LOAD, Index, 2),
            Res
    end.

-define(a(I, J), begin
                     case {sets:is_element({I, J}, Balls),
                           sets:is_element({I, J}, Blocks)} of
                         {true, false} ->
                             ball;
                         {false, true} ->
                             block;
                         {false, false} ->
                             empty
                     end
                 end).

tilt_north(Balls, Blocks, Rows, Cols) ->
    F = fun(Col, {FBallsAcc, FLoadAcc}) ->
                G = fun(Row, {GBallsAcc, GLoadAcc, NextRow}) ->
                            case ?a(Row, Col) of
                                empty ->
                                    {GBallsAcc, GLoadAcc, NextRow};
                                ball ->
                                    Load = Rows + 1 - NextRow,
                                    {sets:add_element({NextRow, Col}, GBallsAcc),
                                     GLoadAcc + Load,
                                     NextRow + 1};
                                block ->
                                    {GBallsAcc, GLoadAcc, Row + 1}
                            end
                    end,
                L = lists:seq(1, Rows),
                Init = {FBallsAcc, FLoadAcc, 1},
                {FBalls, FLoad, _} = lists:foldl(G, Init, L),
                {FBalls, FLoad}
        end,
    lists:foldl(F, {sets:new(), 0}, lists:seq(1, Cols)).

tilt_west(Balls, Blocks, Rows, Cols) ->
    F = fun(Row, {FBallsAcc, FLoadAcc}) ->
                Load = Rows + 1 - Row,
                G = fun(Col, {GBallsAcc, GLoadAcc, NextCol}) ->
                            case ?a(Row, Col) of
                                empty ->
                                    {GBallsAcc, GLoadAcc, NextCol};
                                ball ->
                                    {sets:add_element({Row, NextCol}, GBallsAcc),
                                     GLoadAcc + Load,
                                     NextCol + 1};
                                block ->
                                    {GBallsAcc, GLoadAcc, Col + 1}
                            end
                    end,
                L = lists:seq(1, Cols),
                Init = {FBallsAcc, FLoadAcc, 1},
                {FBalls, FLoad, _} = lists:foldl(G, Init, L),
                {FBalls, FLoad}
        end,
    lists:foldl(F, {sets:new(), 0}, lists:seq(1, Rows)).

tilt_south(Balls, Blocks, Rows, Cols) ->
    F = fun(Col, {FBallsAcc, FLoadAcc}) ->
                G = fun(Row, {GBallsAcc, GLoadAcc, NextRow}) ->
                            case ?a(Row, Col) of
                                empty ->
                                    {GBallsAcc, GLoadAcc, NextRow};
                                ball ->
                                    Load = Rows + 1 - NextRow,
                                    {sets:add_element({NextRow, Col}, GBallsAcc),
                                     GLoadAcc + Load,
                                     NextRow - 1};
                                block ->
                                    {GBallsAcc, GLoadAcc, Row - 1}
                            end
                    end,
                L = lists:seq(Rows, 1, -1),
                Init = {FBallsAcc, FLoadAcc, Rows},
                {FBalls, FLoad, _} = lists:foldl(G, Init, L),
                {FBalls, FLoad}
        end,
    lists:foldl(F, {sets:new(), 0}, lists:seq(1, Cols)).

tilt_east(Balls, Blocks, Rows, Cols) ->
    F = fun(Row, {FBallsAcc, FLoadAcc}) ->
                Load = Rows + 1 - Row,
                G = fun(Col, {GBallsAcc, GLoadAcc, NextCol}) ->
                            case ?a(Row, Col) of
                                empty ->
                                    {GBallsAcc, GLoadAcc, NextCol};
                                ball ->
                                    {sets:add_element({Row, NextCol}, GBallsAcc),
                                     GLoadAcc + Load,
                                     NextCol - 1};
                                block ->
                                    {GBallsAcc, GLoadAcc, Col - 1}
                            end
                    end,
                L = lists:seq(Cols, 1, -1),
                Init = {FBallsAcc, FLoadAcc, Cols},
                {FBalls, FLoad, _} = lists:foldl(G, Init, L),
                {FBalls, FLoad}
        end,
    lists:foldl(F, {sets:new(), 0}, lists:seq(1, Rows)).
