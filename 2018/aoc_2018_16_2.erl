-module(aoc_2018_16_2).

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
        {ok, ["Before:"]} ->
            {ok, BeforeRegs} = io:fread("", " [~d,~d,~d,~d]"),
            {ok, [Op, A, B, C]} = io:fread("", "~d ~d ~d ~d"),
            {ok, AfterRegs} = io:fread("", "After: [~d,~d,~d,~d]"),
            {{Op, A, B, C}, input_regs(BeforeRegs), input_regs(AfterRegs)};
        {ok, [NS]} ->
            {ok, [A, B, C]} = io:fread("", "~d ~d ~d"),
            {list_to_integer(NS), A, B, C}
    end.

input_regs(Regs) ->
    maps:from_list(lists:zip(lists:seq(0, 3), Regs)).

ini() ->
    {[], []}.

do(X) ->
    X.

acc({Samples, Code}, {_, _, _} = Sample) ->
    {[Sample | Samples], Code};
acc({Samples, Code}, {_, _, _, _} = Instruction) ->
    {Samples, [Instruction | Code]}.

fin({Samples, Code}) ->
    Mapping = mapping(Samples),
    F = fun({Op, A, B, C}, Acc) ->
                run(Acc, maps:get(Op, Mapping), A, B, C)
        end,
    Regs = lists:foldr(F, maps:from_list([{X, 0} || X <- lists:seq(0, 3)]), Code),
    maps:get(0, Regs).

-define(INSTRUCTIONS, [addr, addi, mulr, muli, banr, bani, borr, bori,
                       setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]).

mapping(L) ->
    Whole = sets:from_list(?INSTRUCTIONS),
    Init = [{X, Whole} || X <- lists:seq(0, length(?INSTRUCTIONS) - 1)],
    F = fun({{Op, A, B, C}, BeforeRegs, AfterRegs}, Acc) ->
                G = fun(Instruction) ->
                            run(BeforeRegs, Instruction, A, B, C) =:= AfterRegs
                    end,
                maps:put(Op, sets:filter(G, maps:get(Op, Acc)), Acc)
        end,
    Possibles = lists:foldl(F, maps:from_list(Init), L),
    select(Possibles).

-define(REG(X), (X >= 0 andalso X =< 3)).

run(Regs, addr, A, B, C) when ?REG(A), ?REG(B), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) + maps:get(B, Regs), Regs);
run(Regs, addi, A, B, C) when ?REG(A), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) + B, Regs);
run(Regs, mulr, A, B, C) when ?REG(A), ?REG(B), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) * maps:get(B, Regs), Regs);
run(Regs, muli, A, B, C) when ?REG(A), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) * B, Regs);
run(Regs, banr, A, B, C) when ?REG(A), ?REG(B), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) band maps:get(B, Regs), Regs);
run(Regs, bani, A, B, C) when ?REG(A), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) band B, Regs);
run(Regs, borr, A, B, C) when ?REG(A), ?REG(B), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) bor maps:get(B, Regs), Regs);
run(Regs, bori, A, B, C) when ?REG(A), ?REG(C) ->
    maps:put(C, maps:get(A, Regs) bor B, Regs);
run(Regs, setr, A, _, C) when ?REG(A), ?REG(C) ->
    maps:put(C, maps:get(A, Regs), Regs);
run(Regs, seti, A, _, C) when ?REG(A), ?REG(C) ->
    maps:put(C, A, Regs);
run(Regs, gtir, A, B, C) when ?REG(B), ?REG(C) ->
    maps:put(C, case A > maps:get(B, Regs) of true -> 1; false -> 0 end, Regs);
run(Regs, gtri, A, B, C) when ?REG(A), ?REG(C) ->
    maps:put(C, case maps:get(A, Regs) > B of true -> 1; false -> 0 end, Regs);
run(Regs, gtrr, A, B, C) when ?REG(A), ?REG(B), ?REG(C) ->
    V = case maps:get(A, Regs) > maps:get(B, Regs) of true -> 1; false -> 0 end,
    maps:put(C, V, Regs);
run(Regs, eqir, A, B, C) when ?REG(B), ?REG(C) ->
    maps:put(C, case A == maps:get(B, Regs) of true -> 1; false -> 0 end, Regs);
run(Regs, eqri, A, B, C) when ?REG(A), ?REG(C) ->
    maps:put(C, case maps:get(A, Regs) == B of true -> 1; false -> 0 end, Regs);
run(Regs, eqrr, A, B, C) when ?REG(A), ?REG(B), ?REG(C) ->
    V = case maps:get(A, Regs) == maps:get(B, Regs) of true -> 1; false -> 0 end,
    maps:put(C, V, Regs);
run(_, _, _, _, _) ->
    undefined.

select(Possibles) ->
    select_impl(maps:new(), Possibles).

select_impl(Acc, Possibles) ->
    case maps:size(Possibles) of
        0 ->
            Acc;
        _ ->
            {K, V} = single(Possibles),
            F = fun(_, Set) -> sets:del_element(V, Set) end,
            NewPossibles = maps:map(F, maps:remove(K, Possibles)),
            select_impl(maps:put(K, V, Acc), NewPossibles)
    end.

single(Possibles) ->
    single_impl(maps:iterator(Possibles)).

single_impl(Iter) ->
    {K, V, It} = maps:next(Iter),
    case sets:size(V) of
        1 ->
            {K, hd(sets:to_list(V))};
        _ ->
            single_impl(It)
    end.
