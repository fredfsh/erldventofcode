-module(aoc_2017_10_1).

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
    case io:get_line("") of
        eof ->
            eof;
        L ->
            Parts = string:split(string:trim(L, trailing), ",", all),
            [list_to_integer(X) || X <- Parts]
    end.

ini() ->
    0.

-define(LEN, 256).

do(X) ->
    Arr = array:from_list(lists:seq(0, ?LEN - 1)),
    do_impl(X, Arr, 0, 0).

do_impl([], Arr, _, _) ->
    array:get(0, Arr) * array:get(1, Arr);
do_impl([H | T], Arr, Cur, Skip) ->
    N = array:size(Arr),
    do_impl(T, reverse(Arr, H, Cur), (Cur + H + Skip) rem N, Skip + 1).

reverse(Arr, 0, _Cur) ->
    Arr;
reverse(Arr, Rev, Cur) ->
    N = array:size(Arr),
    End = (Cur + Rev - 1) rem N,
    reverse_impl(Arr, Cur, End).

reverse_impl(Arr, Start, Start) ->
    Arr;
reverse_impl(Arr, Start, End) ->
    N = array:size(Arr),
    Arr2 = swap(Arr, Start, End),
    NextStart = (Start + 1) rem N,
    NextEnd = (End + N - 1) rem N,
    case {NextStart, NextEnd} of
        {End, Start} ->
            Arr2;
        _ ->
            reverse_impl(Arr2, NextStart, NextEnd)
    end.

swap(Arr, I, J) ->
    Temp = array:get(I, Arr),
    Arr2 = array:set(I, array:get(J, Arr), Arr),
    array:set(J, Temp, Arr2).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
