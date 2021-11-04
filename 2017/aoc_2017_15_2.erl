-module(aoc_2017_15_2).

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
    case io:fread("", "Generator ~s starts with ~d") of
        eof ->
            eof;
        {ok, [_, X]} ->
            X
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin([B, A]) ->
    matches(0, 0, A, B).

-define(PAIRS, 5000000).
-define(FA, 16807).
-define(FB, 48271).
-define(MOD, 2147483647).
-define(MASK, 2#1111111111111111).  % 16 bits

matches(Acc, ?PAIRS, _, _) ->
    Acc;
matches(Acc, I, LA, LB) ->
    A = gen(LA, ?FA, 4),
    B = gen(LB, ?FB, 8),
    Delta = case A band ?MASK =:= B band ?MASK of
                true ->
                    1;
                _ ->
                    0
            end,
    matches(Acc + Delta, I + 1, A, B).

gen(X, Factor, Multiplier) ->
    N = X * Factor rem ?MOD,
    case N rem Multiplier of
        0 ->
            N;
        _ ->
            gen(N, Factor, Multiplier)
    end.
