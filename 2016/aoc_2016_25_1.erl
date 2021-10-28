-module(aoc_2016_25_1).

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
            input_impl(array:set(array:size(Arr), {jnz, RegOrInt, Int}, Arr));
        {ok, [out]} ->
            {ok, [Out]} = io:fread("", " ~s"),
            RegOrInt = reg_or_int(Out),
            input_impl(array:set(array:size(Arr), {out, RegOrInt}, Arr))
    end.

reg_or_int(S) ->
    case string:to_integer(S) of
        {N, _} when is_integer(N) ->
            N;
        {error, no_integer} ->
            list_to_atom(S)
    end.

do(Arr) ->
    do_impl(0, Arr).

do_impl(A, Arr) ->
    case validate(0, init_regs(A), maps:new(), 0, Arr) of
        true ->
            A;
        false ->
            do_impl(A + 1, Arr)
    end.

init_regs(A) ->
    maps:from_list([{a, A}, {b, 0}, {c, 0}, {d, 0}]).

validate(IP, Regs, States, Expect, Arr) ->
    case array:size(Arr) of
        N when IP >= N ->
            false;
        _ ->
            Instruction = array:get(IP, Arr),
            case execute(Instruction, IP, Regs, States, Expect) of
                true ->
                    true;
                false ->
                    false;
                {NewIP, NewRegs, NewStates, NewExpect} ->
                    validate(NewIP, NewRegs, NewStates, NewExpect, Arr)
            end
    end.

execute({cpy, From, ToReg}, IP, Regs, States, Expect) ->
    {IP + 1, maps:put(ToReg, maps:get(From, Regs, From), Regs), States, Expect};
execute({inc, Reg}, IP, Regs, States, Expect) ->
    {IP + 1, maps:update_with(Reg, fun(X) -> X + 1 end, Regs), States, Expect};
execute({dec, Reg}, IP, Regs, States, Expect) ->
    {IP + 1, maps:update_with(Reg, fun(X) -> X - 1 end, Regs), States, Expect};
execute({jnz, Cond, Int}, IP, Regs, States, Expect) ->
    case maps:get(Cond, Regs, Cond) of
        0 ->
            {IP + 1, Regs, States, Expect};
        _ ->
            {IP + Int, Regs, States, Expect}
    end;
execute({out, Out}, IP, Regs, States, Expect) ->
    case maps:get(Out, Regs, Out) of
        Expect ->
            State = state(IP, Regs),
            case sets:is_element(State, States) of
                true ->
                    true;
                false ->
                    {IP + 1, Regs, sets:add_element(State, States), 1 - Expect}
            end;
        _ ->
            false
    end.

state(IP, Regs) ->
    {IP,
     maps:get(a, Regs),
     maps:get(b, Regs),
     maps:get(c, Regs),
     maps:get(d, Regs)}.
