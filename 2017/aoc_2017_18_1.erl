-module(aoc_2017_18_1).

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
    case io:fread("", "~a") of
        eof ->
            eof;
        {ok, [Cmd]} when Cmd =:= snd; Cmd =:= rcv ->
            input_single_arg(Cmd);
        {ok, [Cmd]} ->
            input_double_arg(Cmd)
    end.

input_single_arg(Cmd) ->
    {ok, [S]} = io:fread("", " ~s"),
    {Cmd, reg_or_int(S)}.

reg_or_int(S) ->
    case string:to_integer(S) of
        {error, no_integer} ->
            list_to_atom(S);
        {N, _} ->
            N
    end.

input_double_arg(Cmd) ->
    {ok, [S1, S2]} = io:fread("", " ~s ~s"),
    {Cmd, reg_or_int(S1), reg_or_int(S2)}.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(X) ->
    run(undefined, 0, maps:new(), X).

run(Last, IP, Regs, Code) ->
    case array:get(IP, Code) of
        {snd, X} ->
            run(val(X, Regs), IP + 1, Regs, Code);
        {set, X, Y} ->
            run(Last, IP + 1, maps:put(X, val(Y, Regs), Regs), Code);
        {add, X, Y} ->
            O = val(Y, Regs),
            F = fun(V) -> V + O end,
            run(Last, IP + 1, maps:update_with(X, F, O, Regs), Code);
        {mul, X, Y} ->
            O = val(Y, Regs),
            F = fun(V) -> V * O end,
            run(Last, IP + 1, maps:update_with(X, F, 0, Regs), Code);
        {mod, X, Y} ->
            O = val(Y, Regs),
            F = fun(V) -> V rem O end,
            run(Last, IP + 1, maps:update_with(X, F, 0, Regs), Code);
        {rcv, X} ->
            case val(X, Regs) of
                0 ->
                    run(Last, IP + 1, Regs, Code);
                _ ->
                    Last
            end;
        {jgz, X, Y} ->
            Jmp = case val(X, Regs) of
                      N when N > 0 ->
                          val(Y, Regs);
                      _ ->
                          1
                  end,
            run(Last, IP + Jmp, Regs, Code)
    end.

val(X, _Regs) when is_integer(X) ->
    X;
val(X, Regs) ->
    maps:get(X, Regs, 0).
