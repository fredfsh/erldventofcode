-module(aoc_2015_17_2).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl([]).

input_impl(L) ->
    case io:fread("", "~d") of
        eof ->
            L;
        {ok, [C]} ->
            input_impl([C | L])
    end.

-define(VOL, 150).
-define(TAB, memo).

do(L) ->
    ets:new(?TAB, [named_table]),
    {_N, Cnt} = lists:min(memo(?VOL, L)),
    Cnt.

memo(Vol, L) ->
    N = length(L),
    Key = {Vol, N},
    case ets:lookup(?TAB, Key) of
        [{_, X}] ->
            X;
        [] ->
            Y = memo_impl(Vol, L),
            ets:insert(?TAB, {Key, Y}),
            Y
    end.

memo_impl(0, _) ->
    [{0, 1}];
memo_impl(_, []) ->
    0;
memo_impl(X, [Y | T]) when Y > X ->
    memo(X, T);
memo_impl(X, [Y | T]) ->
    PL1 = memo(X, T),
    PL2 = memo(X - Y, T),
    merge(PL1, inc(PL2)).

inc(0) ->
    0;
inc(PL) ->
    [{N + 1, Cnt} || {N, Cnt} <- PL].

merge(0, PL) ->
    PL;
merge(PL, 0) ->
    PL;
merge(PL1, PL2) ->
    F = fun({N, Cnt}, PL) ->
                case proplists:lookup(N, PL) of
                    none ->
                        [{N, Cnt} | PL];
                    {N, X} ->
                        [{N, X + Cnt} | proplists:delete(N, PL)]
                end
        end,
    lists:foldl(F, PL1, PL2).
