-module(aoc_2020_22_2).

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
    {_, Q} = game(Q1, Q2),
    score(Q).

game(Q1, Q2) ->
    game_impl(Q1, Q2, sets:new()).

game_impl(Q1, Q2, Seen) ->
%%    io:format("~p~n~p~n~n", [Q1, Q2]),
    case sets:is_element({queue:to_list(Q1), queue:to_list(Q2)}, Seen) of
        true ->
            {1, Q1};
        _ ->
            case {queue:is_empty(Q1), queue:is_empty(Q2)} of
                {true, _} ->
                    {2, Q2};
                {_, true} ->
                    {1, Q1};
                _ ->
                    NewSeen = sets:add_element(
                                {queue:to_list(Q1), queue:to_list(Q2)},
                                Seen),
                    {NQ1, NQ2} = play(Q1, Q2),
                    game_impl(NQ1, NQ2, NewSeen)
            end
    end.

play(Q1, Q2) ->
    {{value, X1}, NQ1} = queue:out(Q1),
    {{value, X2}, NQ2} = queue:out(Q2),
    case queue:len(NQ1) >= X1 andalso queue:len(NQ2) >= X2 of
        true ->
            {RQ1, _} = queue:split(X1, NQ1),
            {RQ2, _} = queue:split(X2, NQ2),
            case game_impl(RQ1, RQ2, sets:new()) of
                {1, _} ->
                    {queue:in(X2, queue:in(X1, NQ1)), NQ2};
                _ ->
                    {NQ1, queue:in(X1, queue:in(X2, NQ2))}
            end;
        _ ->
            case X1 > X2 of
                true ->
                    {queue:in(X2, queue:in(X1, NQ1)), NQ2};
                _ ->
                    {NQ1, queue:in(X1, queue:in(X2, NQ2))}
            end
    end.

score(Q) ->
%%    io:format("~p~n", [Q]),
    score_impl(Q, 0, 1).

score_impl(Q, Acc, Coef) ->
    case queue:out_r(Q) of
        {empty, _} ->
            Acc;
        {{value, X}, Q2} ->
            score_impl(Q2, Acc + X * Coef, Coef + 1)
    end.
