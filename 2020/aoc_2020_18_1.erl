-module(aoc_2020_18_1).

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
    case io:get_line("") of
        eof ->
            eof;
        L ->
            L
    end.

do(X) ->
    do_impl(X, [], []).

-define(SPACE, 32).
-define(NEW_LINE, 10).

do_impl([?NEW_LINE], [Acc], []) ->
    Acc;
do_impl([?SPACE | T], Numbers, Ops) ->
    do_impl(T, Numbers, Ops);
do_impl([X | _] = S, Numbers, Ops) when X >= $0, X =< $9 ->
    {N, Rem} = string:to_integer(S),
    {NewNumbers, NewOps} = calc([N | Numbers], Ops),
    do_impl(Rem, NewNumbers, NewOps);
do_impl([$) | T], Numbers, [$( | OpsT]) ->
    {NewNumbers, NewOps} = calc(Numbers, OpsT),
    do_impl(T, NewNumbers, NewOps);
do_impl([Op | T], Numbers, Ops) ->
    do_impl(T, Numbers, [Op | Ops]).

calc([L, R | NT], [$+ | OT]) ->
    calc([L + R | NT], OT);
calc([L, R | NT], [$* | OT]) ->
    calc([L * R | NT], OT);
calc(Numbers, Ops) ->
    {Numbers, Ops}.
