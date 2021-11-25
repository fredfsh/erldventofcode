-module(aoc_2019_7_2).

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
        {ok, [X]} ->
            L = [list_to_integer(Y) || Y <- string:split(X, ",", all)],
            array:from_list(L)
    end.

ini() ->
    0.

-record(process, {input, output, id, ip = 0, code}).

-define(AMPS, 5).

do(X) ->
    Phases = lists:seq(?AMPS, ?AMPS * 2 - 1),
    PhaseSeqs = perms(Phases),
    Process = #process{ip = 0, code = X},
    F = fun(PhaseSeq, Acc) ->
                max(Acc, sim(Process, PhaseSeq))
        end,
    lists:foldl(F, sim(Process, Phases), PhaseSeqs).

perms([]) -> [[]];
perms(L) -> [[H | T] || H <- L, T <- perms(L -- [H])].

sim(Process, PhaseSeq) ->
    L = lists:zip(lists:seq($a, $e), PhaseSeq),
    [#process{input = [I]} = H | T] = [Process#process{input = [P], id = ID}
                                       || {ID, P} <- L],
    run([H#process{input = [I, 0]} | T]).

-define(OP_HALT, 99).
-define(OP_ADD, 1).
-define(OP_MUL, 2).
-define(OP_IN, 3).
-define(OP_OUT, 4).
-define(OP_JTR, 5).
-define(OP_JFS, 6).
-define(OP_LT, 7).
-define(OP_EQ, 8).

run([H | T]) ->
    case run(H) of
        {halt, #process{output = Output, id = $e}} ->
            Output;
        {halt, _} ->
            run(T);
        #process{output = Output} = P ->
            case T of
                [] ->
                    run([P]);
                [#process{input = Input} = H2 | T2] ->
                    run([H2#process{input = Input ++ [Output]} | T2] ++ [P])
            end
    end;
run(Process) ->
    case instruction(Process) of
        {?OP_HALT, []} ->
            {halt, Process};
        {?OP_OUT, _} = Instruction ->
            execute(Instruction, Process);
        Instruction ->
            run(execute(Instruction, Process))
    end.

-define(V(X), array:get(X, Code)).

instruction(#process{ip = IP, code = Code}) ->
    Op = ?V(IP) rem 100,
    {Op, parameters(IP, Code, nparameters(Op))}.

nparameters(?OP_HALT) -> 0;
nparameters(?OP_ADD) -> 3;
nparameters(?OP_MUL) -> 3;
nparameters(?OP_IN) -> 1;
nparameters(?OP_OUT) -> 1;
nparameters(?OP_JTR) -> 2;
nparameters(?OP_JFS) -> 2;
nparameters(?OP_LT) -> 3;
nparameters(?OP_EQ) -> 3.

parameters(IP, Code, P) ->
    parameters_impl([], ?V(IP) div 100, IP + 1, P, Code).

parameters_impl(Acc, _, _, 0, _) ->
    lists:reverse(Acc);
parameters_impl(Acc, Modes, Pos, P, Code) ->
    Mode = Modes rem 10,
    parameters_impl([{?V(Pos), Mode} | Acc], Modes div 10, Pos+1, P-1, Code).

execute({?OP_ADD, [PA, PB, {C, _}]}, #process{code = Code, ip = IP} = Process) ->
    V = val(PA, Code) + val(PB, Code),
    Process#process{ip = IP + 4, code = array:set(C, V, Code)};
execute({?OP_MUL, [PA, PB, {C, _}]}, #process{code = Code, ip = IP} = Process) ->
    V = val(PA, Code) * val(PB, Code),
    Process#process{ip = IP + 4, code = array:set(C, V, Code)};
execute({?OP_IN, [{P, _}]},
        #process{input = [H | T], code = Code, ip = IP} = Process) ->
    Process#process{input = T, ip = IP + 2, code = array:set(P, H, Code)};
execute({?OP_OUT, [P]}, #process{ip = IP, code = Code} = Process) ->
    Process#process{ip = IP + 2, output = val(P, Code)};
execute({?OP_JTR, [PA, PB]}, #process{code = Code, ip = IP} = Process) ->
    NewIP = case val(PA, Code) of
                0 ->
                    IP + 3;
                _ ->
                    val(PB, Code)
            end,
    Process#process{ip = NewIP};
execute({?OP_JFS, [PA, PB]}, #process{code = Code, ip = IP} = Process) ->
    NewIP = case val(PA, Code) of
                0 ->
                    val(PB, Code);
                _ ->
                    IP + 3
            end,
    Process#process{ip = NewIP};
execute({?OP_LT, [PA, PB, {C, _}]}, #process{code = Code, ip = IP} = Process) ->
    V = case val(PA, Code) < val(PB, Code) of
            true ->
                1;
            false ->
                0
        end,
    Process#process{ip = IP + 4, code = array:set(C, V, Code)};
execute({?OP_EQ, [PA, PB, {C, _}]}, #process{code = Code, ip = IP} = Process) ->
    V = case val(PA, Code) =:= val(PB, Code) of
            true ->
                1;
            false ->
                0
        end,
    Process#process{ip = IP + 4, code = array:set(C, V, Code)}.

-define(MODE_POSITION, 0).
-define(MODE_IMMEDIATE, 1).

val({Parameter, ?MODE_POSITION}, Code) ->
    ?V(Parameter);
val({Parameter, ?MODE_IMMEDIATE}, _) ->
    Parameter.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
