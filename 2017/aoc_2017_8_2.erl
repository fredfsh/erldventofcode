-module(aoc_2017_8_2).

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
    case io:fread("", "~a ~a ~d if ~a ~a ~d") of
        eof ->
            eof;
        {ok, Instruction} ->
            Instruction
    end.

ini() ->
    {maps:new(), undefined}.

do(X) ->
    X.

acc({Acc, Max}, [Reg, IncDec, Amount, CondReg, Op, Pivot]) ->
    NewAcc =
        case {condition(maps:get(CondReg, Acc, 0), Op, Pivot), IncDec} of
            {true, inc} ->
                maps:update_with(Reg, fun(X) -> X + Amount end, Amount, Acc);
            {true, dec} ->
                maps:update_with(Reg, fun(X) -> X - Amount end, -Amount, Acc);
            {false, _} ->
                Acc
        end,
    NewMax = case {maps:values(Acc), Max} of
                 {[], _} ->
                     Max;
                 {L, undefined} ->
                     lists:max(L);
                 {L, _} ->
                     max(lists:max(L), Max)
             end,
    {NewAcc, NewMax}.

condition(Int, '>', Pivot) ->
    Int > Pivot;
condition(Int, '<', Pivot) ->
    Int < Pivot;
condition(Int, '>=', Pivot) ->
    Int >= Pivot;
condition(Int, '<=', Pivot) ->
    Int =< Pivot;
condition(Int, '==', Pivot) ->
    Int =:= Pivot;
condition(Int, '!=', Pivot) ->
    Int =/= Pivot.

fin({_, X}) ->
    X.
