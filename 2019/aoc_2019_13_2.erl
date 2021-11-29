-module(aoc_2019_13_2).

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

-record(process, {input = [], output = [], ip = 0, rb = 0, code}).
-record(state, {score, blocks = sets:new(), paddle, ball}).

do(X) ->
    run(#process{code = array:set(0, 2, X)}, #state{paddle = {undef, undef}}).

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

run(#process{} = Process,
    #state{score = undefined, paddle = {PX, _}, ball = Ball} = State) ->
    case instruction(Process) of
        {?OP_OUT, _} = Instruction ->
            NewProcess = execute(Instruction, Process),
            NewState = process_output(NewProcess, State),
            run(NewProcess, NewState);
        {?OP_IN, _} = Instruction ->
            Input = case Ball of
                        PX ->
                            0;
                        _ ->
                            (Ball - PX) div abs(Ball - PX)
                    end,
            run(execute(Instruction, Process#process{input = Input}), State);
        Instruction ->
            run(execute(Instruction, Process), State)
    end;
run(_, #state{score = Score}) ->
    Score.

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
execute({?OP_IN, [P]}, #process{input=In, ip=IP, rb=RB, code=Code} = Process) ->
    Process#process{ip = IP + 2, code = array:set(?ADDR(P), In, Code)};
execute({?OP_OUT, [P]}, #process{output=O, ip=IP, rb=RB, code=Code} = Process) ->
    Process#process{ip = IP + 2, output = [?VAL(P) | O]};
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

-define(TILE_BLOCK, 2).
-define(TILE_PADDLE, 3).
-define(TILE_BALL, 4).

process_output(#process{output = [Score, 0, -1 | T]},
               #state{blocks = Blocks} = State) when length(T) rem 3 =:= 0 ->
    case sets:size(Blocks) of
        0 ->
            State#state{score = Score};
        _ ->
            State
    end;
process_output(#process{output = [?TILE_BLOCK, Y, X | T]},
               #state{blocks = Blocks} = State) when length(T) rem 3 =:= 0 ->
    State#state{blocks = sets:add_element({X, Y}, Blocks)};
process_output(#process{output = [?TILE_PADDLE, Y, X | T]},
               #state{blocks = Blocks} = State) when length(T) rem 3 =:= 0 ->
    State#state{paddle = {X, Y}, blocks = sets:del_element({X, Y}, Blocks)};
process_output(#process{output = [?TILE_BALL, Y, X | T]},
               #state{blocks = Blocks} = State) when length(T) rem 3 =:= 0 ->
    State#state{ball = X, blocks = sets:del_element({X, Y}, Blocks)};
process_output(#process{output = [_, Y, X | T]},
               #state{blocks = Blocks} = State) when length(T) rem 3 =:= 0 ->
    State#state{blocks = sets:del_element({X, Y}, Blocks)};
process_output(_, State) ->
    State.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
