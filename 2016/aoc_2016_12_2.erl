-module(aoc_2016_12_2).

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
            {ok, [Cond, Int]} = io:fread("", " ~s ~d"),
            RegOrInt = reg_or_int(Cond),
            input_impl(array:set(array:size(Arr), {jnz, RegOrInt, Int}, Arr))
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

do_impl(IP, Regs, Arr) ->
    case array:size(Arr) of
        N when IP >= N ->
            maps:get(a, Regs);
        _ ->
            Instruction = array:get(IP, Arr),
            {NewIP, NewRegs} = execute(Instruction, IP, Regs),
            do_impl(NewIP, NewRegs, Arr)
    end.

execute({cpy, Int, Reg}, IP, Regs) when is_integer(Int) ->
    {IP + 1, maps:put(Reg, Int, Regs)};
execute({cpy, FromReg, ToReg}, IP, Regs) ->
    {IP + 1, maps:put(ToReg, maps:get(FromReg, Regs), Regs)};
execute({inc, Reg}, IP, Regs) ->
    {IP + 1, maps:update_with(Reg, fun(X) -> X + 1 end, Regs)};
execute({dec, Reg}, IP, Regs) ->
    {IP + 1, maps:update_with(Reg, fun(X) -> X - 1 end, Regs)};
execute({jnz, 0, _Int}, IP, Regs) ->
    {IP + 1, Regs};
execute({jnz, CondInt, JmpInt}, IP, Regs)
  when is_integer(CondInt), CondInt =/= 0 ->
    {IP + JmpInt, Regs};
execute({jnz, Reg, Int}, IP, Regs) ->
    case maps:get(Reg, Regs) of
        0 ->
            {IP + 1, Regs};
        _ ->
            {IP + Int, Regs}
    end.

init_regs() ->
    maps:from_list([{a, 0}, {b, 0}, {c, 1}, {d, 0}]).
