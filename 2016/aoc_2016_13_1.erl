-module(aoc_2016_13_1).

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
    {Q, Closed} = visit(1, 1, 0, queue:new(), sets:new(), X),
    do_impl(Q, Closed, X).

visit(X, Y, Steps, Q, Closed, Magic) when X >= 0, Y >= 0 ->
    case sets:is_element({X, Y}, Closed) of
        true ->
            {Q, Closed};
        false ->
            NewQ = case wall(X, Y, Magic) of
                       true ->
                           Q;
                       false ->
                           queue:in({X, Y, Steps}, Q)
                   end,
            {NewQ, sets:add_element({X, Y}, Closed)}
    end;
visit(_, _, _, Q, Closed, _) ->
    {Q, Closed}.

wall(X, Y, Magic) ->
    ones(X*X + 3*X + 2*X*Y + Y + Y*Y + Magic) band 1 =:= 1.

ones(X) ->
    ones_impl(X, 0).

ones_impl(0, N) ->
    N;
ones_impl(X, N) ->
    ones_impl(X bsr 1, N + (X band 1)).

-define(X, 31).
-define(Y, 39).

-define(D, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).

do_impl(Q, Closed, Magic) ->
    {{value, {X, Y, Steps}}, Q2} = queue:out(Q),
    F = fun({DX, DY}, {QAcc, ClosedAcc}) ->
                case {X + DX, Y + DY} of
                    {?X, ?Y} ->
                        Steps + 1;
                    _ ->
                        visit(X + DX, Y + DY, Steps + 1, QAcc, ClosedAcc, Magic)
                end;
           (_, N) ->
                N
        end,
    case lists:foldl(F, {Q2, Closed}, ?D) of
        {NewQ, NewClosed} ->
            do_impl(NewQ, NewClosed, Magic);
        N ->
            N
    end.
