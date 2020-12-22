-module(aoc_2020_22_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Q1 = input(),
    Q2 = input(),
    do(Q1, Q2).

input() ->
    io:get_line(""),
    input_impl(queue:new()).

input_impl(Q) ->
    case io:get_line("") of
        eof ->
            Q;
        "\n" ->
            Q;
        L ->
            {N, _} = string:to_integer(L),
            input_impl(queue:in(N, Q))
    end.

do(Q1, Q2) ->
    case {queue:is_empty(Q1), queue:is_empty(Q2)} of
        {true, _} ->
            score(Q2);
        {_, true} ->
            score(Q1);
        _ ->
            {{value, X1}, NQ1} = queue:out(Q1),
            {{value, X2}, NQ2} = queue:out(Q2),
            {NewQ1, NewQ2} = case X1 > X2 of
                                 true ->
                                     {queue:in(X2, queue:in(X1, NQ1)), NQ2};
                                 _ ->
                                     {NQ1, queue:in(X1, queue:in(X2, NQ2))}
                             end,
            do(NewQ1, NewQ2)
    end.

score(Q) ->
    score_impl(Q, 0, 1).

score_impl(Q, Acc, Coef) ->
    case queue:out_r(Q) of
        {empty, _} ->
            Acc;
        {{value, X}, Q2} ->
            score_impl(Q2, Acc + X * Coef, Coef + 1)
    end.
