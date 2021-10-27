-module(aoc_2016_23_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(array:new()).

input_impl(Arr) ->
    case io:fread("", "~a") of
        eof ->
            Arr;
        {ok, [cpy]} ->
            {ok, [From, Reg]} = io:fread("", " ~s ~a"),
            RegOrInt = reg_or_int(From),
            input_impl(array:set(array:size(Arr), {cpy, RegOrInt, Reg}, Arr));
        {ok, [IncOrDec]} when IncOrDec =:= inc; IncOrDec =:= dec ->
            {ok, [Reg]} = io:fread("", " ~a"),
            input_impl(array:set(array:size(Arr), {IncOrDec, Reg}, Arr));
        {ok, [jnz]} ->
            {ok, [CondS, JmpS]} = io:fread("", " ~s ~s"),
            Cond = reg_or_int(CondS),
            Jmp = reg_or_int(JmpS),
            input_impl(array:set(array:size(Arr), {jnz, Cond, Jmp}, Arr));
        {ok, [tgl]} ->
            {ok, [Reg]} = io:fread("", " ~a"),
            input_impl(array:set(array:size(Arr), {tgl, Reg}, Arr))
    end.

reg_or_int(S) ->
    case string:to_integer(S) of
        {N, _} when is_integer(N) ->
            N;
        {error, no_integer} ->
            list_to_atom(S)
    end.

do(Arr) ->
    do_impl(0, init_regs(), Arr).

init_regs() ->
    maps:from_list([{a, 12}, {b, 0}, {c, 0}, {d, 0}]).

do_impl(IP, Regs, Arr) ->
    case array:size(Arr) of
        N when IP >= N ->
            maps:get(a, Regs);
        _ ->
            Instruction = array:get(IP, Arr),
            {NewIP, NewRegs, NewArr} = execute(Instruction, IP, Regs, Arr),
            do_impl(NewIP, NewRegs, NewArr)
    end.

execute({cpy, Int, Reg}, IP, Regs, Arr) when is_integer(Int), is_atom(Reg) ->
    {IP + 1, maps:put(Reg, Int, Regs), Arr};
execute({cpy, FromReg, ToReg}, IP, Regs, Arr) when is_atom(FromReg),
                                                   is_atom(ToReg) ->
    {IP + 1, maps:put(ToReg, maps:get(FromReg, Regs), Regs), Arr};
execute({inc, Reg}, IP, Regs, Arr) when is_atom(Reg) ->
    {IP + 1, maps:update_with(Reg, fun(X) -> X + 1 end, Regs), Arr};
execute({dec, Reg}, IP, Regs, Arr) when is_atom(Reg) ->
    {IP + 1, maps:update_with(Reg, fun(X) -> X - 1 end, Regs), Arr};
execute({jnz, Cond, Jmp}, IP, Regs, Arr) ->
    case maps:get(Cond, Regs, Cond) of
        0 ->
            {IP + 1, Regs, Arr};
        _ ->
            {IP + maps:get(Jmp, Regs, Jmp), Regs, Arr}
    end;
execute({tgl, Reg}, IP, Regs, Arr) when is_atom(Reg) ->
    NewArr = case {maps:get(Reg, Regs) + IP, array:size(Arr)} of
                 {X, N} when X < 0; X >= N ->
                     Arr;
                 {X, _} ->
                     Instruction = toggle(array:get(X, Arr)),
                     array:set(X, Instruction, Arr)
             end,
    {IP + 1, Regs, NewArr};
execute(_, IP, Regs, Arr) ->
    {IP + 1, Regs, Arr}.

toggle({inc, X}) ->
    {dec, X};
toggle({_, X}) ->
    {inc, X};
toggle({jnz, X, Y}) ->
    {cpy, X, Y};
toggle({_, X, Y}) ->
    {jnz, X, Y}.
