-module(aoc_2023_14_1).

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
            array:from_list(string:trim(L))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(a(I), array:get(I, Arr)).

fin(Matrix) ->
    Cols = array:size(array:get(0, Matrix)),
    Rows = array:size(Matrix),
    F = fun(X, FAcc) ->
                G = fun(Y, Arr, {GAcc, Load}) ->
                            case ?a(X) of
                                $. ->
                                    {GAcc, Load};
                                $O ->
                                    %%io:format("load(~p,~p) = ~p~n", [X, Y, Load]),
                                    {GAcc + Load, Load - 1};
                                $# ->
                                    {GAcc, Rows - Y - 1}
                            end
                    end,
                {Res, _} = array:foldl(G, {FAcc, Rows}, Matrix),
                Res
        end,
    lists:foldl(F, 0, lists:seq(0, Cols - 1)).
