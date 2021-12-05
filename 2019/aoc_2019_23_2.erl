-module(aoc_2019_23_2).

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
            array:from_list(L, 0)
    end.

ini() ->
    0.

-record(process, {input = [], output = [], id, ip = 0, rb = 0, code}).
-record(nat, {last = {u, u}, sent = {u, u}, idles = 0}).

-define(PROCESSES, 50).

do(Code) ->
    IDs = lists:seq(0, ?PROCESSES - 1),
    MQ = maps:from_list([{I, [I]} || I <- IDs]),
    Processes = [#process{id = I, code = Code} || I <- IDs],
    run(queue:from_list(Processes), MQ, #nat{}).

-define(OP_HALT, 99).
-define(OP_ADD, 1).
-define(OP_MUL, 2).
-define(OP_IN, 3).
-define(OP_OUT, 4).
-define(OP_JTR, 5).
-define(OP_JFS, 6).
-define(OP_LT, 7).
-define(OP_EQ, 8).
-define(OP_REL, 9).

-define(NAT_ADDR, 255).
-define(IDLE_THRESHOLD, 10000).

run(Q, MQ, #nat{last = {LX, LY}, sent = {_, SY}, idles = Idles} = NAT) ->
    {{value, #process{id = ID, input = Input} = Process}, Q2} = queue:out(Q),
    case instruction(Process) of
        {?OP_HALT, _} ->
            run(Q2, MQ, NAT);
        {?OP_IN, _} when Idles > ?IDLE_THRESHOLD, LY =:= SY ->
            SY;
        {?OP_IN, _} when Idles > ?IDLE_THRESHOLD ->
            Q3 = queue:in(Process, Q2),
            MQ2 = maps:put(0, [LX, LY], MQ),
            run(Q3, MQ2, NAT#nat{sent = {LX, LY}, idles = 0});
        {?OP_IN, _} = Instruction ->
            {Inputs, NAT2} = case maps:get(ID, MQ, undefined) of
                                 undefined ->
                                     {Input, NAT#nat{idles = Idles + 1}};
                                 Messages ->
                                     {Input ++ Messages, NAT#nat{idles = 0}}
                             end,
            Process2 = execute(Instruction, Process#process{input = Inputs}),
            run(queue:in(Process2, Q2), maps:remove(ID, MQ), NAT2);
        {?OP_OUT, _} = Instruction ->
            case execute(Instruction, Process) of
                #process{output = [Y, X, ?NAT_ADDR]} = Process2 ->
                    Q3 = queue:in(Process2#process{output = []}, Q2),
                    run(Q3, MQ, NAT#nat{last = {X, Y}, idles = 0});
                #process{output = [Y, X, Addr]} = Process2 ->
                    Q3 = queue:in(Process2#process{output = []}, Q2),
                    F = fun(L) -> L ++ [X, Y] end,
                    MQ2 = maps:update_with(Addr, F, [X, Y], MQ),
                    run(Q3, MQ2, NAT#nat{idles = 0});
                Process2 ->
                    run(queue:in(Process2, Q2), MQ, NAT#nat{idles = 0})
            end;
        Instruction ->
            Process2 = execute(Instruction, Process),
            run(queue:in(Process2, Q2), MQ, NAT)
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
nparameters(?OP_EQ) -> 3;
nparameters(?OP_REL) -> 1.

parameters(IP, Code, P) ->
    parameters_impl([], ?V(IP) div 100, IP + 1, P, Code).

parameters_impl(Acc, _, _, 0, _) ->
    lists:reverse(Acc);
parameters_impl(Acc, Modes, Pos, P, Code) ->
    Mode = Modes rem 10,
    parameters_impl([{?V(Pos), Mode} | Acc], Modes div 10, Pos+1, P-1, Code).

-define(PROCESS, #process{ip = IP, rb = RB, code = Code} = Process).
-define(ADDR(X), addr(X, RB)).
-define(VAL(X), val(X, RB, Code)).

execute({?OP_ADD, [PA, PB, PC]}, ?PROCESS) ->
    V = ?VAL(PA) + ?VAL(PB),
    Process#process{ip = IP + 4, code = array:set(?ADDR(PC), V, Code)};
execute({?OP_MUL, [PA, PB, PC]}, ?PROCESS) ->
    V = ?VAL(PA) * ?VAL(PB),
    Process#process{ip = IP + 4, code = array:set(?ADDR(PC), V, Code)};
execute({?OP_IN, [P]},
        #process{input = [], ip = IP, rb = RB, code = Code} = Process) ->
    Process#process{ip = IP + 2, code = array:set(?ADDR(P), -1, Code)};
execute({?OP_IN, [P]},
        #process{input = [H | T], ip = IP, rb = RB, code = Code} = Process) ->
    Process#process{input = T, ip = IP + 2, code = array:set(?ADDR(P), H, Code)};
execute({?OP_OUT, [P]},
        #process{output = Output, ip = IP, rb = RB, code = Code} = Process) ->
    Process#process{ip = IP + 2, output = [?VAL(P) | Output]};
execute({?OP_JTR, [PA, PB]}, ?PROCESS) ->
    NewIP = case ?VAL(PA) of
                0 ->
                    IP + 3;
                _ ->
                    ?VAL(PB)
            end,
    Process#process{ip = NewIP};
execute({?OP_JFS, [PA, PB]}, ?PROCESS) ->
    NewIP = case ?VAL(PA) of
                0 ->
                    ?VAL(PB);
                _ ->
                    IP + 3
            end,
    Process#process{ip = NewIP};
execute({?OP_LT, [PA, PB, PC]}, ?PROCESS) ->
    V = case ?VAL(PA) < ?VAL(PB) of
            true ->
                1;
            false ->
                0
        end,
    Process#process{ip = IP + 4, code = array:set(?ADDR(PC), V, Code)};
execute({?OP_EQ, [PA, PB, PC]}, ?PROCESS) ->
    V = case ?VAL(PA) =:= ?VAL(PB) of
            true ->
                1;
            false ->
                0
        end,
    Process#process{ip = IP + 4, code = array:set(?ADDR(PC), V, Code)};
execute({?OP_REL, [P]}, #process{code = Code, ip = IP, rb = RB} = Process) ->
    Process#process{ip = IP + 2, rb = RB + ?VAL(P)}.

-define(MODE_POSITION, 0).
-define(MODE_IMMEDIATE, 1).
-define(MODE_RELATIVE, 2).

val({Parameter, ?MODE_IMMEDIATE}, _, _) ->
    Parameter;
val(P, RB, Code) ->
    ?V(addr(P, RB)).

addr({Parameter, ?MODE_POSITION}, _) ->
    Parameter;
addr({Parameter, ?MODE_RELATIVE}, RB) ->
    RB + Parameter.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
