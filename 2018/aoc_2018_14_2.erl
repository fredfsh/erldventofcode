-module(aoc_2018_14_2).

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
            X
    end.

ini() ->
    0.

-define(FIRST, 3).
-define(SECOND, 7).

do(X) ->
    score(array:from_list([?FIRST, ?SECOND]), 0, 1, X).

score(Arr, First, Second, X) ->
    case appear(X, Arr) of
        false ->
            R1 = array:get(First, Arr),
            R2 = array:get(Second, Arr),
            N = array:size(Arr),
            case R1 + R2 of
                Sum when Sum < 10 ->
                    score(array:set(N, Sum, Arr),
                          (First + R1 + 1) rem (N + 1),
                          (Second + R2 + 1) rem (N + 1),
                          X);
                S ->
                    Arr2 = array:set(N, S div 10, Arr),
                    case appear(X, Arr2) of
                        false ->
                            score(array:set(N + 1, S rem 10, Arr2),
                                  (First + R1 + 1) rem (N + 2),
                                  (Second + R2 + 1) rem (N + 2),
                                  X);
                        Res ->
                            Res
                    end
            end;
        Res ->
            Res
    end.

appear(L, Arr) ->
    appear_impl(lists:reverse(L), array:size(Arr) - 1, Arr).

appear_impl([], I, _) ->
    I + 1;
appear_impl(_, -1, _) ->
    false;
appear_impl([H | T], I, Arr) ->
    case array:get(I, Arr) =:= H - $0 of
        true ->
            appear_impl(T, I - 1, Arr);
        false ->
            false
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
