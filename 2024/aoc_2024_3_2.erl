-module(aoc_2024_3_2).

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
    {0, true}.

do(X) ->
    X.

acc({Acc, Enabled}, X) ->
    %% the enable/disable state is inherited across multiple lines in the input
    {Res, NewEnabled} = acc_impl(0, Enabled, X),
    {Acc + Res, NewEnabled}.

acc_impl(Acc, Enabled, []) ->
    {Acc, Enabled};
acc_impl(Acc, true, [$m, $u, $l, $( | T]) ->
    mul_first_op(Acc, T);
acc_impl(Acc, true, [$d, $o, $n, $', $t, $(, $) | T]) ->
    %%io:format("disabling~n"),
    acc_impl(Acc, false, T);
acc_impl(Acc, false, [$d, $o, $(, $) | T]) ->
    %%io:format("enabling~n"),
    acc_impl(Acc, true, T);
acc_impl(Acc, Enabled, L) ->
    acc_impl(Acc, Enabled, tl(L)).

mul_first_op(Acc, [H | T]) when H >= $0 andalso H =< $9 ->
    mul_first_op_impl(Acc, H - $0, T);
mul_first_op(Acc, L) ->
    acc_impl(Acc, true, L).

mul_first_op_impl(Acc, Op, [$, | T]) ->
    mul_second_op(Acc, T, Op);
mul_first_op_impl(Acc, Op, [H | T]) when H >= $0 andalso H =< $9 ->
    mul_first_op_impl(Acc, Op * 10 + (H - $0), T);
mul_first_op_impl(Acc, _, L) ->
    acc_impl(Acc, true, L).

mul_second_op(Acc, [H | T], FirstOp) when H >= $0 andalso H =< $9 ->
    mul_second_op_impl(Acc, H - $0, T, FirstOp);
mul_second_op(Acc, L, _) ->
    acc_impl(Acc, true, L).

mul_second_op_impl(Acc, Op, [$) | T], FirstOp) ->
    %%io:format("Adding ~p*~p~n", [FirstOp, Op]),
    acc_impl(Acc + FirstOp * Op, true, T);
mul_second_op_impl(Acc, Op, [H | T], FirstOp) when H >= $0 andalso H =< $9 ->
    mul_second_op_impl(Acc, Op * 10 + (H - $0), T, FirstOp);
mul_second_op_impl(Acc, _, L, _) ->
    acc_impl(Acc, true, L).

fin({X, _}) ->
    X.
