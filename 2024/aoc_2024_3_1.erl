-module(aoc_2024_3_1).

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
            L
    end.

ini() ->
    0.

do(X) ->
    do_impl(0, X).

do_impl(Acc, []) ->
    Acc;
do_impl(Acc, [$m, $u, $l, $( | T]) ->
    mul_first_op(Acc, T);
do_impl(Acc, L) ->
    do_impl(Acc, tl(L)).

mul_first_op(Acc, [H | T]) when H >= $0 andalso H =< $9 ->
    mul_first_op_impl(Acc, H - $0, T);
mul_first_op(Acc, L) ->
    do_impl(Acc, L).

mul_first_op_impl(Acc, Op, [$, | T]) ->
    mul_second_op(Acc, T, Op);
mul_first_op_impl(Acc, Op, [H | T]) when H >= $0 andalso H =< $9 ->
    mul_first_op_impl(Acc, Op * 10 + (H - $0), T);
mul_first_op_impl(Acc, _, L) ->
    do_impl(Acc, L).

mul_second_op(Acc, [H | T], FirstOp) when H >= $0 andalso H =< $9 ->
    mul_second_op_impl(Acc, H - $0, T, FirstOp);
mul_second_op(Acc, L, _) ->
    do_impl(Acc, L).

mul_second_op_impl(Acc, Op, [$) | T], FirstOp) ->
    do_impl(Acc + FirstOp * Op, T);
mul_second_op_impl(Acc, Op, [H | T], FirstOp) when H >= $0 andalso H =< $9 ->
    mul_second_op_impl(Acc, Op * 10 + (H - $0), T, FirstOp);
mul_second_op_impl(Acc, _, L, _) ->
    do_impl(Acc, L).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
