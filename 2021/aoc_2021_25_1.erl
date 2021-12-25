-module(aoc_2021_25_1).

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
            array:from_list(X)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(X) ->
    sim(1, X).

sim(Steps, Graph) ->
    case move(Graph) of
        {_, 0} ->
            Steps;
        {NewGraph, _} ->
            sim(Steps + 1, NewGraph)
    end.

move(Graph) ->
    {NewGraph, Moves} = move_rights(Graph, 0),
    move_downs(NewGraph, Moves).

move_rights(Graph, MovesAcc) ->
    Cols = array:size(array:get(0, Graph)),
    F = fun(Y, Arr, {GraphAcc, FMovesAcc}) ->
                G = fun(X, $v, {GAcc, MAcc}) ->
                            {array:set(X, $v, GAcc), MAcc};
                       (X, $>, {GAcc, MAcc}) ->
                            case array:get((X + 1) rem Cols, Arr) of
                                $. ->
                                    {array:set(X, $., GAcc), MAcc + 1};
                                _ ->
                                    {array:set(X, $>, GAcc), MAcc}
                            end;
                       (X, $., {GAcc, MAcc}) ->
                            case array:get((X + Cols - 1) rem Cols, Arr) of
                                $> ->
                                    {array:set(X, $>, GAcc), MAcc};
                                _ ->
                                    {array:set(X, $., GAcc), MAcc}
                            end
                    end,
                {NewArr, Moves} = array:foldl(G, {array:new(), FMovesAcc}, Arr),
                {array:set(Y, NewArr, GraphAcc), Moves}
        end,
    array:foldl(F, {array:new(), MovesAcc}, Graph).

-define(A(X, Y, Arr), array:get(X, array:get(Y, Arr))).

move_downs(Graph, MovesAcc) ->
    Rows = array:size(Graph),
    F = fun(Y, Arr, {GraphAcc, FMovesAcc}) ->
                G = fun(X, $>, {GAcc, MAcc}) ->
                            {array:set(X, $>, GAcc), MAcc};
                       (X, $v, {GAcc, MAcc}) ->
                            case ?A(X, (Y + 1) rem Rows, Graph) of
                                $. ->
                                    {array:set(X, $., GAcc), MAcc + 1};
                                _ ->
                                    {array:set(X, $v, GAcc), MAcc}
                            end;
                       (X, $., {GAcc, MAcc}) ->
                            case ?A(X, (Y + Rows - 1) rem Rows, Graph) of
                                $v ->
                                    {array:set(X, $v, GAcc), MAcc};
                                _ ->
                                    {array:set(X, $., GAcc), MAcc}
                            end
                    end,
                {NewArr, Moves} = array:foldl(G, {array:new(), FMovesAcc}, Arr),
                {array:set(Y, NewArr, GraphAcc), Moves}
        end,
    array:foldl(F, {array:new(), MovesAcc}, Graph).
