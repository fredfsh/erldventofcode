-module(aoc_2023_23_1).

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
        {ok, [S]} ->
            array:from_list(S)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Matrix) ->
    XY = {start(Matrix), 0},
    dfs(XY, sets:from_list([XY]), Matrix).

start(Matrix) ->
    start_impl(0, array:get(0, Matrix)).

start_impl(X, Arr) ->
    case array:get(X, Arr) of
        $. ->
            X;
        _ ->
            start_impl(X + 1, Arr)
    end.

dfs({_, Y} = XY, Visited, Matrix) ->
    case array:size(Matrix) - 1 of
        Y ->
            sets:size(Visited) - 1;
        _ ->
            F = fun({NXY, NewVisited}, Acc) ->
                        max(Acc, dfs(NXY, NewVisited, Matrix))
                end,
            lists:foldl(F, 0, nexts(XY, Visited, Matrix))
    end.

-define(Ds, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).
-define(VISITED(X, Y), sets:is_element({X, Y}, Visited)).
-define(a(X, Y), array:get(X, array:get(Y, Matrix))).

nexts({X, Y}, Visited, Matrix) ->
    Rows = array:size(Matrix),
    Cols = array:size(array:get(0, Matrix)),
    F = fun({DX, DY}, Acc) ->
                case {X + DX, Y + DY} of
                    {NX, _} when NX < 0; NX >= Cols ->
                        Acc;
                    {_, NY} when NY < 0; NY >= Rows ->
                        Acc;
                    {NX, NY} = NXY ->
                        %% Observed no northwards (^) or westwards (<) slopes
                        %% Observed no slopes terminated into other slopes
                        %% Observed no slopes terminated into forests
                        %% Observed no slopes on edge
                        case {?VISITED(NX, NY), ?a(NX, NY)} of
                            {true, _} ->
                                Acc;
                            {false, $#} ->
                                Acc;
                            {false, $.} ->
                                NewVisited = sets:add_element(NXY, Visited),
                                [{NXY, NewVisited} | Acc];
                            {false, $>} ->
                                {NNX, NNY} = NNXY = {NX + 1, NY},
                                case {?VISITED(NNX, NNY), ?a(NNX, NNY)} of
                                    {true, _} ->
                                        Acc;
                                    {false, $#} ->
                                        Acc;
                                    _ ->
                                        Set = sets:from_list([NXY, NNXY]),
                                        NewVisited = sets:union(Set, Visited),
                                        [{NNXY, NewVisited} | Acc]
                                end;
                            {false, $v} ->
                                {NNX, NNY} = NNXY = {NX, NY + 1},
                                case {?VISITED(NNX, NNY), ?a(NNX, NNY)} of
                                    {true, _} ->
                                        Acc;
                                    {false, $#} ->
                                        Acc;
                                    _ ->
                                        Set = sets:from_list([NXY, NNXY]),
                                        NewVisited = sets:union(Set, Visited),
                                        [{NNXY, NewVisited} | Acc]
                                end
                        end
                end
        end,
    lists:foldl(F, [], ?Ds).
