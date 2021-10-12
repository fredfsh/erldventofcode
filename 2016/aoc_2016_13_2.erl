-module(aoc_2016_13_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    {Count, Q, Closed} = visit(1, 1, 0, 0, queue:new(), sets:new(), X),
    do_impl(Count, Q, Closed, X).

-define(STEPS, 50).

visit(X, Y, Steps, Count, Q, Closed, Magic)
  when X >= 0, Y >= 0, Steps =< ?STEPS ->
    case sets:is_element({X, Y}, Closed) of
        true ->
            {Count, Q, Closed};
        false ->
            {NewCount, NewQ} = case wall(X, Y, Magic) of
                                   true ->
                                       {Count, Q};
                                   false ->
                                       {Count + 1, queue:in({X, Y, Steps}, Q)}
                               end,
            {NewCount, NewQ, sets:add_element({X, Y}, Closed)}
    end;
visit(_, _, _, Count, Q, Closed, _) ->
    {Count, Q, Closed}.

wall(X, Y, Magic) ->
    ones(X*X + 3*X + 2*X*Y + Y + Y*Y + Magic) band 1 =:= 1.

ones(X) ->
    ones_impl(X, 0).

ones_impl(0, N) ->
    N;
ones_impl(X, N) ->
    ones_impl(X bsr 1, N + (X band 1)).

-define(D, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).

do_impl(Count, Q, Closed, Magic) ->
    case queue:out(Q) of
        {empty, _} ->
            Count;
        {{value, {X, Y, Steps}}, Q2} ->
            F = fun({DX, DY}, {CountAcc, QAcc, ClosedAcc}) ->
                        visit(X + DX, Y + DY, Steps + 1,
                              CountAcc, QAcc, ClosedAcc, Magic)
                end,
            {NewCount, NewQ, NewClosed} =
                lists:foldl(F, {Count, Q2, Closed}, ?D),
            do_impl(NewCount, NewQ, NewClosed, Magic)
    end.
