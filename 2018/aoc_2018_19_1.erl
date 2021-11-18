-module(aoc_2018_19_1).

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
    case io:fread("", "~a ~d ~d ~d") of
        eof ->
            eof;
        {ok, [Op, A, B, C]} ->
            {Op, A, B, C}
    end.

ini() ->
    {ok, [IPReg]} = io:fread("", "#ip ~d"),
    {IPReg, array:new()}.

do(X) ->
    X.

acc({IPReg, Code}, X) ->
    {IPReg, array:set(array:size(Code), X, Code)}.

fin({IPReg, Code}) ->
    Regs = maps:from_list([{R, 0} || R <- lists:seq(0, 5)]),
    run(0, Regs, Code, IPReg).

run(IP, Regs, Code, IPReg) ->
    Regs2 = maps:put(IPReg, IP, Regs),
    case IP < 0 orelse IP >= array:size(Code) of
        true ->
            maps:get(0, Regs2);
        false ->
            {Op, A, B, C} = array:get(IP, Code),
            Regs3 = execute(Regs2, Op, A, B, C),
            run(maps:get(IPReg, Regs3) + 1, Regs3, Code, IPReg)
    end.

execute(Regs, addr, A, B, C) ->
    maps:put(C, maps:get(A, Regs) + maps:get(B, Regs), Regs);
execute(Regs, addi, A, B, C) ->
    maps:put(C, maps:get(A, Regs) + B, Regs);
execute(Regs, mulr, A, B, C) ->
    maps:put(C, maps:get(A, Regs) * maps:get(B, Regs), Regs);
execute(Regs, muli, A, B, C) ->
    maps:put(C, maps:get(A, Regs) * B, Regs);
execute(Regs, banr, A, B, C) ->
    maps:put(C, maps:get(A, Regs) band maps:get(B, Regs), Regs);
execute(Regs, bani, A, B, C) ->
    maps:put(C, maps:get(A, Regs) band B, Regs);
execute(Regs, borr, A, B, C) ->
    maps:put(C, maps:get(A, Regs) bor maps:get(B, Regs), Regs);
execute(Regs, bori, A, B, C) ->
    maps:put(C, maps:get(A, Regs) bor B, Regs);
execute(Regs, setr, A, _, C) ->
    maps:put(C, maps:get(A, Regs), Regs);
execute(Regs, seti, A, _, C) ->
    maps:put(C, A, Regs);
execute(Regs, gtir, A, B, C) ->
    maps:put(C, case A > maps:get(B, Regs) of true -> 1; false -> 0 end, Regs);
execute(Regs, gtri, A, B, C) ->
    maps:put(C, case maps:get(A, Regs) > B of true -> 1; false -> 0 end, Regs);
execute(Regs, gtrr, A, B, C) ->
    V = case maps:get(A, Regs) > maps:get(B, Regs) of true -> 1; false -> 0 end,
    maps:put(C, V, Regs);
execute(Regs, eqir, A, B, C) ->
    maps:put(C, case A == maps:get(B, Regs) of true -> 1; false -> 0 end, Regs);
execute(Regs, eqri, A, B, C) ->
    maps:put(C, case maps:get(A, Regs) == B of true -> 1; false -> 0 end, Regs);
execute(Regs, eqrr, A, B, C) ->
    V = case maps:get(A, Regs) == maps:get(B, Regs) of true -> 1; false -> 0 end,
    maps:put(C, V, Regs).
