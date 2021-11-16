-module(aoc_2018_14_1).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    [].

-define(FIRST, 3).
-define(SECOND, 7).

do(X) ->
    score(array:from_list([?FIRST, ?SECOND]), 0, 1, X).

-define(N, 10).

score(Arr, First, Second, X) ->
    N = array:size(Arr),
    case N - X >= ?N of
        true ->
            [array:get(I, Arr) + $0 || I <- lists:seq(X, X + ?N - 1)];
        false ->
            R1 = array:get(First, Arr),
            R2 = array:get(Second, Arr),
            NewArr =
                case R1 + R2 of
                    Sum when Sum < 10 ->
                        array:set(N, Sum, Arr);
                    S ->
                        array:set(N + 1, S rem 10, array:set(N, S div 10, Arr))
                end,
            N2 = array:size(NewArr),
            score(NewArr, (First + R1 + 1) rem N2, (Second + R2 + 1) rem N2, X)
    end.

acc(Acc, X) ->
    Acc ++ X.

fin(X) ->
    X.
