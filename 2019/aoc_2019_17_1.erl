-module(aoc_2019_17_1).

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

-record(process, {input, output, ip = 0, rb = 0, code}).
-record(state, {x, y, dx, dy, ox = 0, oy = 0, scaffolds = sets:new()}).

do(X) ->
    run(#process{code = X}, #state{}).

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

run(Process, #state{scaffolds = Scaffolds} = State) ->
    case instruction(Process) of
        {?OP_HALT, _} ->
            intersections(Scaffolds);
        {?OP_OUT, _} = Instruction ->
            Process2 = execute(Instruction, Process),
            #process{output = Output} = Process2,
            State2 = process_output(Output, State),
            run(Process2, State2);
        Instruction ->
            run(execute(Instruction, Process), State)
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

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

intersections(Scaffolds) ->
    F = fun({X, Y}, Acc) ->
                G = fun({DX, DY}) ->
                            sets:is_element({X + DX, Y + DY}, Scaffolds)
                    end,
                case lists:all(G, ?D) of
                    true ->
                        Acc + X * Y;
                    false ->
                        Acc
                end
        end,
    sets:fold(F, 0, Scaffolds).

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
        #process{input = Input, ip = IP, rb = RB, code = Code} = Process) ->
    Process#process{input = undefined,
                    ip = IP + 2,
                    code = array:set(?ADDR(P), Input, Code)};
execute({?OP_OUT, [P]}, #process{ip = IP, rb = RB, code = Code} = Process) ->
    Process#process{ip = IP + 2, output = ?VAL(P)};
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

-define(NEWLINE, 10).

process_output($#, #state{ox = OX, oy = OY, scaffolds = Scaffolds} = State) ->
    State#state{ox = OX + 1, scaffolds = sets:add_element({OX, OY}, Scaffolds)};
process_output($., #state{ox = OX} = State) ->
    State#state{ox = OX + 1};
process_output(?NEWLINE, #state{oy = OY} = State) ->
    State#state{ox = 0, oy = OY + 1};
process_output(D, #state{ox = OX, oy = OY, scaffolds = Scaffolds} = State) ->
    {DX, DY} = direction(D),
    State#state{x = OX, y = OY, dx = DX, dy = DY, ox = OX + 1,
                scaffolds = sets:add_element({OX, OY}, Scaffolds)}.

direction($^) -> { 0, -1};
direction($v) -> { 0,  1};
direction($<) -> {-1,  0};
direction($>) -> { 1,  0}.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
