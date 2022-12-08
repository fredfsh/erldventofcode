-module(aoc_2022_8_2).

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
    F = fun(Y, _, Acc) when Y =:= 0; Y =:= M - 1 ->
                Acc;
           (Y, Row, Acc) ->
                G = fun(X, _, GAcc) when X =:= 0; X =:= N - 1 ->
                            GAcc;
                       (X, Height, GAcc) ->
                            Score = distance({X - 1, 0, -1}, Y, Height, Arr) *
                                distance({X + 1, N - 1, 1}, Y, Height, Arr) *
                                distance(X, {Y - 1, 0, -1}, Height, Arr) *
                                distance(X, {Y + 1, M - 1, 1}, Height, Arr),
                            max(GAcc, Score)
                    end,
                array:foldl(G, Acc, Row)
        end,
    array:foldl(F, 0, Arr).

-define(arr(X, Y, A), array:get(X, array:get(Y, A))).

distance({Start, End, DX}, Y, Height, Arr) ->
    distance_impl(0, {Start, End, DX}, Y, Height, Arr);
distance(X, {Start, End, DY}, Height, Arr) ->
    distance_impl(0, X, {Start, End, DY}, Height, Arr).

distance_impl(Acc, {Start, End, DX}, Y, Height, Arr) ->
    case ?arr(Start, Y, Arr) >= Height of
        true ->
            Acc + 1;
        false when Start =:= End ->
            Acc + 1;
        _ ->
            distance_impl(Acc + 1, {Start + DX, End, DX}, Y, Height, Arr)
    end;
distance_impl(Acc, X, {Start, End, DY}, Height, Arr) ->
    case ?arr(X, Start, Arr) >= Height of
        true ->
            Acc + 1;
        false when Start =:= End ->
            Acc + 1;
        _ ->
            distance_impl(Acc + 1, X, {Start + DY, End, DY}, Height, Arr)
    end.
