-module(aoc_2019_8_1).

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

-define(SIZE, 25 * 6).

do(X) ->
    F = fun(C, {LayersAcc, LayerAcc}) ->
                NewLayerAcc = inc(C, LayerAcc),
                case sum(NewLayerAcc) of
                    ?SIZE ->
                        {[NewLayerAcc | LayersAcc], {0, 0, 0}};
                    _ ->
                        {LayersAcc, NewLayerAcc}
                end
        end,
    {Layers, _} = lists:foldl(F, {[], {0, 0, 0}}, X),
    {_, Acc1, Acc2} = hd(lists:sort(Layers)),
    Acc1 * Acc2.

inc($0, {Acc0, Acc1, Acc2}) ->
    {Acc0 + 1, Acc1, Acc2};
inc($1, {Acc0, Acc1, Acc2}) ->
    {Acc0, Acc1 + 1, Acc2};
inc($2, {Acc0, Acc1, Acc2}) ->
    {Acc0, Acc1, Acc2 + 1}.

sum({Acc0, Acc1, Acc2}) ->
    Acc0 + Acc1 + Acc2.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
