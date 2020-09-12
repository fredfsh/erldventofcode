-module(aoc_2015_24_2).

-export([start/0]).

-define(TAB, memo).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl(0, []).

input_impl(S, L) ->
    case io:fread("", "~d") of
        eof ->
            {S, L};
        {ok, [N]} ->
            input_impl(S + N, [N | L])
    end.

do({S, L}) ->
    ets:new(?TAB, [named_table]),
    do_impl(S div 4, L).

do_impl(S, L) ->
    {_MinN, MinP} = dp(S, L),
    MinP.

dp(Rem, L) ->
    case ets:lookup(?TAB, {length(L), Rem}) of
        [] ->
            Res = memo(Rem, L),
            ets:insert(?TAB, {{length(L), Rem}, Res}),
            Res;
        [{_, Res}] ->
            Res
    end.

memo(Rem, L) ->
    memo_impl({length(L), undefined}, {Rem, 0, 1}, L).

memo_impl({MinN, MinP}, {0, N, Product}, _L) ->
    result(MinN, MinP, N, Product);
memo_impl({MinN, MinP}, {_Rem, _N, _Product}, []) ->
    {MinN, MinP};
memo_impl({MinN, MinP}, {Rem, N, Product}, [X | T]) ->
    {Acc1N, Acc1P} =
        case dp(Rem, T) of
            {_, undefined} ->
                {MinN, MinP};
            {Res1N, Res1P} ->
                result(MinN, MinP, N + Res1N, Product * Res1P)
        end,
    case Rem >= X of
        false ->
            {Acc1N, Acc1P};
        true ->
            case dp(Rem - X, T) of
                {_, undefined} ->
                    {Acc1N, Acc1P};
                {Res2N, Res2P} ->
                    result(Acc1N, Acc1P, N + 1 + Res2N, Product * X * Res2P)
            end
    end.

result(MinN, _MinP, N, Product) when N < MinN ->
    {N, Product};
result(MinN, MinP, MinN, Product) when Product < MinP ->
    {MinN, Product};
result(MinN, MinP, _, _) ->
    {MinN, MinP}.
