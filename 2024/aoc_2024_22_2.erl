-module(aoc_2024_22_2).

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

-define(TAB, acc).

ini() ->
    ets:new(?TAB, [named_table]),
    0.

-define(ITERS, 2000).

do(X) ->
    do_impl(X, {}, sets:new(), ?ITERS).

do_impl(_, _, _, 0) ->
    ok;
do_impl(X, History, Seen, Iters) ->
    Y = next(X),
    Price = Y rem 10,
    Change = Price - X rem 10,
    case erlang:append_element(History, Change) of
        Quintuple when tuple_size(Quintuple) =:= 5 ->
            Quadruple = erlang:delete_element(1, Quintuple),
            NewSeen = inc(Seen, Quadruple, Price),
            do_impl(Y, Quadruple, NewSeen, Iters - 1);
        Quadruple when tuple_size(Quadruple) =:= 4 ->
            NewSeen = inc(Seen, Quadruple, Price),
            do_impl(Y, Quadruple, NewSeen, Iters - 1);
        Tuple ->
            do_impl(Y, Tuple, Seen, Iters - 1)
    end.

inc(Seen, Quadruple, Price) ->
    case sets:is_element(Quadruple, Seen) of
        true ->
            Seen;
        false ->
            ets:update_counter(?TAB, Quadruple, Price, {Quadruple, 0}),
            sets:add_element(Quadruple, Seen)
    end.

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

acc(_, _) ->
    ok.

fin(_) ->
    F = fun({_, V}, Acc) -> max(V, Acc) end,
    ets:foldl(F, 0, ?TAB).
