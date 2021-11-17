-module(aoc_2018_16_1).

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
        {ok, [_]} ->
            io:fread("", "~d ~d ~d"),
            ignore
    end.

input_regs(Regs) ->
    maps:from_list(lists:zip(lists:seq(0, 3), Regs)).

ini() ->
    [].

do(X) ->
    X.

acc(Acc, ignore) ->
    Acc;
acc(Acc, X) ->
    [X | Acc].

fin(L) ->
    length(lists:filter(fun(X) -> behave(X) >= 3 end, L)).

-define(INSTRUCTIONS, [addr, addi, mulr, muli, banr, bani, borr, bori,
                       setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]).

behave({{_, A, B, C}, BeforeRegs, AfterRegs}) ->
    F = fun(Ins) -> run(BeforeRegs, Ins, A, B, C) =:= AfterRegs end,
    length(lists:filter(F, ?INSTRUCTIONS)).

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
