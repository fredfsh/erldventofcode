-module(aoc_2022_8_1).

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
            array:from_list(lists:map(fun(Y) -> Y - $0 end, X))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Arr) ->
    M = array:size(Arr),
    N = array:size(array:get(0, Arr)),
    F = fun(Y, Row, Acc) ->
                G = fun(X, Height, GAcc) ->
                            case smaller(Height, Arr, {0, X - 1}, Y) orelse
                                smaller(Height, Arr, {X + 1, N - 1}, Y) orelse
                                smaller(Height, Arr, X, {0, Y - 1}) orelse
                                smaller(Height, Arr, X, {Y + 1, M - 1}) of
                                true ->
                                    GAcc + 1;
                                false ->
                                    GAcc
                            end
                    end,
                array:foldl(G, Acc, Row)
        end,
    array:foldl(F, 0, Arr).

-define(arr(X, Y, A), array:get(X, array:get(Y, A))).

smaller(_Height, _Arr, {Start, End}, _Y) when Start > End ->
    true;
smaller(_Height, _Arr, _X, {Start, End}) when Start > End ->
    true;
smaller(Height, Arr, {Start, End}, Y) ->
    F = fun(X) ->
                ?arr(X, Y, Arr) < Height
        end,
    lists:all(F, lists:seq(Start, End));
smaller(Height, Arr, X, {Start, End}) ->
    F = fun(Y) ->
                ?arr(X, Y, Arr) < Height
        end,
    lists:all(F, lists:seq(Start, End)).
