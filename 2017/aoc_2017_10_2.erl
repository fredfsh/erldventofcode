-module(aoc_2017_10_2).

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

-define(EXTRAS, [17, 31, 73, 47, 23]).

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            string:trim(L, trailing) ++ ?EXTRAS
    end.

ini() ->
    [].

-define(LEN, 256).

do(X) ->
    Arr = array:from_list(lists:seq(0, ?LEN - 1)),
    Sparse = array:to_list(sparse(0, X, Arr, 0, 0, X)),
    Dense = dense(Sparse),
    hex(Dense).

-define(ROUNDS, 64).

sparse(?ROUNDS, _, Arr, _, _, _) ->
    Arr;
sparse(Round, [], Arr, Cur, Skip, Revs) ->
    sparse(Round + 1, Revs, Arr, Cur, Skip, Revs);
sparse(Round, [H | T], Arr, Cur, Skip, Revs) ->
    N = array:size(Arr),
    Reversed = reverse(Arr, H, Cur),
    sparse(Round, T, Reversed, (Cur + H + Skip) rem N, Skip + 1, Revs).

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

-define(WIDTH, 16).

dense(L) ->
    dense_impl([], L).

dense_impl(Acc, []) ->
    lists:reverse(Acc);
dense_impl(Acc, L) ->
    {Left, Right} = lists:split(?WIDTH, L),
    F = fun(X, FAcc) -> FAcc bxor X end,
    dense_impl([lists:foldl(F, 0, Left) | Acc], Right).

hex(L) when is_list(L) ->
    lists:append([hex(X) || X <- L]);
hex(X) ->
    [base16(X div 16), base16(X rem 16)].

base16(X) when X < 10 ->
    X + $0;
base16(X) ->
    X - 10 + $a.

acc(Acc, X) ->
    Acc ++ X.

fin(X) ->
    X.
