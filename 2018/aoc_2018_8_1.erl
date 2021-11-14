-module(aoc_2018_8_1).

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

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    {Res, []} = input_node(0, lists:reverse(X)),
    Res.

input_node(Acc, []) ->
    {Acc, []};
input_node(Acc, [Children, Metas | T]) ->
    {Acc2, T2} = input_children(Acc, T, Children),
    input_metas(Acc2, T2, Metas).

input_children(Sum, L, Children) ->
    F = fun(_, {SumAcc, LAcc}) ->
                input_node(SumAcc, LAcc)
        end,
    lists:foldl(F, {Sum, L}, lists:seq(1, Children)).

input_metas(Sum, L, Metas) ->
    F = fun(_, {Acc, [H | T]}) ->
                {Acc + H, T}
        end,
    lists:foldl(F, {Sum, L}, lists:seq(1, Metas)).
