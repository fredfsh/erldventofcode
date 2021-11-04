-module(aoc_2017_18_2).

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
    Regs0 = maps:put(p, 0, maps:new()),
    Regs1 = maps:put(p, 1, maps:new()),
    run(0, 1, {0, Regs0, []}, {0, Regs1, []}, X).

run(Acc, Thread, {IP0, Regs0, Q0} = State0, {IP1, Regs1, Q1} = State1, X) ->
    {NewAcc, NewThread, NewState0, NewState1} =
        case Thread of
            0 ->
                {Msgs, NewState} = run_thread([], State0, X),
                {Acc, 1, NewState, {IP1, Regs1, Q1 ++ Msgs}};
            1 ->
                {Msgs, NewState} = run_thread([], State1, X),
                {Acc + length(Msgs), 0, {IP0, Regs0, Q0 ++ Msgs}, NewState}
        end,
    case terminated(NewState0, NewState1, X) of
        true ->
            NewAcc;
        false ->
            run(NewAcc, NewThread, NewState0, NewState1, X)
    end.

run_thread(Msgs, {IP, Regs, Q} = State, Code) ->
    case IP < 0 orelse IP >= array:size(Code) of
        true ->
            {Msgs, State};
        false ->
            case array:get(IP, Code) of
                {snd, X} ->
                    run_thread(Msgs ++ [val(X, Regs)], {IP + 1, Regs, Q}, Code);
                {set, X, Y} ->
                    NewState = {IP + 1, maps:put(X, val(Y, Regs), Regs), Q},
                    run_thread(Msgs, NewState, Code);
                {add, X, Y} ->
                    O = val(Y, Regs),
                    F = fun(V) -> V + O end,
                    NewState = {IP + 1, maps:update_with(X, F, O, Regs), Q},
                    run_thread(Msgs, NewState, Code);
                {mul, X, Y} ->
                    O = val(Y, Regs),
                    F = fun(V) -> V * O end,
                    NewState = {IP + 1, maps:update_with(X, F, 0, Regs), Q},
                    run_thread(Msgs, NewState, Code);
                {mod, X, Y} ->
                    O = val(Y, Regs),
                    F = fun(V) -> V rem O end,
                    NewState = {IP + 1, maps:update_with(X, F, 0, Regs), Q},
                    run_thread(Msgs, NewState, Code);
                {rcv, X} ->
                    case Q of
                        [] ->
                            {Msgs, State};
                        [H | T] ->
                            NewState = {IP + 1, maps:put(X, H, Regs), T},
                            run_thread(Msgs, NewState, Code)
                    end;
                {jgz, X, Y} ->
                    Jmp = case val(X, Regs) of
                              N when N > 0 ->
                                  val(Y, Regs);
                              _ ->
                                  1
                          end,
                    run_thread(Msgs, {IP + Jmp, Regs, Q}, Code)
            end
    end.

val(X, _Regs) when is_integer(X) ->
    X;
val(X, Regs) ->
    maps:get(X, Regs, 0).

terminated(S0, S1, Code) ->
    terminated_or_stuck(S0, Code) andalso terminated_or_stuck(S1, Code).

terminated_or_stuck({IP, _, Q}, Code) ->
    case IP < 0 orelse IP >= array:size(Code) of
        true ->
            true;
        _ ->
            case {array:get(IP, Code), Q} of
                {{rcv, _}, []} ->
                    true;
                _ ->
                    false
            end
    end.
