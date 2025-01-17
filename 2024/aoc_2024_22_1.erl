-module(aoc_2024_22_1).

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
    0.

-define(ITERS, 2000).

do(X) ->
    F = fun(_, Acc) ->
                next(Acc)
        end,
    lists:foldl(F, X, lists:seq(1, ?ITERS)).

-define(DIV, 'div').
-define(MUL, '*').

next(X) ->
    next_impl(X, [{?MUL, 64}, {?DIV, 32}, {?MUL, 2048}]).

next_impl(Acc, []) ->
    Acc;
next_impl(Acc, [H | T]) ->
    next_impl(process(Acc, H), T).

-define(MIX(X, Y), X bxor Y).

-define(MASK, 16777216).
-define(PRUNE(X), X band (?MASK - 1)).

process(X, Op) ->
    Y = process_impl(X, Op),
    Z = ?MIX(X, Y),
    ?PRUNE(Z).

process_impl(X, {?DIV, Y}) ->
    X bsr log(Y);
process_impl(X, {?MUL, Y}) ->
    X bsl log(Y).

log(32) -> 5;
log(64) -> 6;
log(2048) -> 11.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
