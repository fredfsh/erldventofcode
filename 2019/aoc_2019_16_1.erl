-module(aoc_2019_16_1).

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
            [C - $0 || C <- X]
    end.

ini() ->
    "".

-define(PHASES, 100).

do(X) ->
    M = matrix(length(X)),
    L = lists:foldl(fun(_, Acc) -> mmul(M, Acc) end, X, lists:seq(1, ?PHASES)),
    [C + $0 || C <- lists:sublist(L, 8)].

matrix(N) ->
    [pattern(I, N) || I <- lists:seq(1, N)].

pattern(I, N) ->
    pattern_impl([], 0, 1, I, N).

pattern_impl(Acc, _, _, _, N) when length(Acc) =:= N ->
    lists:reverse(Acc);
pattern_impl(Acc, Cur, I, I, N) ->
    Next = (Cur + 1) rem 4,
    pattern_impl([d(Next) | Acc], Next, 1, I, N);
pattern_impl(Acc, Cur, Count, I, N) ->
    pattern_impl([d(Cur) | Acc], Cur, Count + 1, I, N).

d(0) -> 0;
d(1) -> 1;
d(2) -> 0;
d(3) -> -1.

mmul(M, X) ->
    [mul(L, X) || L <- M].

mul(L, X) ->
    F = fun(A, {Sum, [H | T]}) ->
                {Sum + A * H, T}
        end,
    {Sum, _} = lists:foldl(F, {0, X}, L),
    abs(Sum) rem 10.

acc(Acc, X) ->
    Acc ++ X.

fin(X) ->
    X.
