-module(aoc_2017_23_1).

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
    case io:fread("", "~a ~s ~s") of
        eof ->
            eof;
        {ok, [Cmd, X, Y]} ->
            {Cmd, reg_or_int(X), reg_or_int(Y)}
    end.

reg_or_int(S) ->
    case string:to_integer(S) of
        {error, no_integer} ->
            list_to_atom(S);
        {N, _} ->
            N
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(X) ->
    run(0, 0, maps:new(), X).

run(Acc, IP, Regs, Code) ->
    case IP < 0 orelse IP >= array:size(Code) of
        true ->
            Acc;
        false ->
            {NewAcc, NewIP, NewRegs} =
                case array:get(IP, Code) of
                    {set, X, Y} ->
                        {Acc, IP + 1, maps:put(X, val(Y, Regs), Regs)};
                    {sub, X, Y} ->
                        O = val(Y, Regs),
                        F = fun(V) -> V - O end,
                        {Acc, IP + 1, maps:update_with(X, F, -O, Regs)};
                    {mul, X, Y} ->
                        F = fun(V) -> V * val(Y, Regs) end,
                        {Acc + 1, IP + 1, maps:update_with(X, F, 0, Regs)};
                    {jnz, X, Y} ->
                        Jmp = case val(X, Regs) of
                                  0 ->
                                      1;
                                  _ ->
                                      val(Y, Regs)
                              end,
                        {Acc, IP + Jmp, Regs}
                end,
            run(NewAcc, NewIP, NewRegs, Code)
    end.

val(X, _Regs) when is_integer(X) ->
    X;
val(X, Regs) ->
    maps:get(X, Regs, 0).
