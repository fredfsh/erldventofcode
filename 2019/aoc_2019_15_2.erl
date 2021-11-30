-module(aoc_2019_15_2).

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

do(Code) ->
    Process = run(#process{code = Code}),
    Set = sets:from_list([{0, 0}]),
    Process2 = locate(queue:from_list([{Process, 0, 0}]), Set),
    bfs(queue:from_list([{Process2, 0, 0, 0}]), Set).

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

run(#process{input = Input} = Process) ->
    case instruction(Process) of
        {?OP_IN, _} when Input =:= undefined ->
            Process;
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

-define(MOVE_NORTH, 1).
-define(MOVE_SOUTH, 2).
-define(MOVE_WEST, 3).
-define(MOVE_EAST, 4).
-define(MOVES, [?MOVE_NORTH, ?MOVE_SOUTH, ?MOVE_WEST, ?MOVE_EAST]).

-define(STATUS_UNMOVED, 0).
-define(STATUS_MOVED, 1).
-define(STATUS_FOUND, 2).

locate(Q, Visited) ->
    {{value, {Process, X, Y}}, Q2} = queue:out(Q),
    F = fun(Move, {QAcc, VisitedAcc} = Acc) ->
                {NX, NY} = move(X, Y, Move),
                case sets:is_element({NX, NY}, VisitedAcc) of
                    true ->
                        Acc;
                    false ->
                        case run(Process#process{input = Move}) of
                            #process{output = ?STATUS_UNMOVED} ->
                                {QAcc, sets:add_element({NX, NY}, VisitedAcc)};
                            #process{output = ?STATUS_MOVED} = P ->
                                {queue:in({P, NX, NY}, QAcc),
                                 sets:add_element({NX, NY}, VisitedAcc)};
                            #process{output = ?STATUS_FOUND} = P ->
                                P
                        end
                end;
           (_, Res) ->
                Res
        end,
    case lists:foldl(F, {Q2, Visited}, ?MOVES) of
        {NewQ, NewVisited} ->
            locate(NewQ, NewVisited);
        Res ->
            Res
    end.

move(X, Y, ?MOVE_NORTH) ->
    {X, Y - 1};
move(X, Y, ?MOVE_SOUTH) ->
    {X, Y + 1};
move(X, Y, ?MOVE_WEST) ->
    {X - 1, Y};
move(X, Y, ?MOVE_EAST) ->
    {X + 1, Y}.

bfs(Q, Visited) ->
    {{value, {Process, X, Y, N}}, Q2} = queue:out(Q),
    F = fun(Move, {QAcc, VisitedAcc} = Acc) ->
                {NX, NY} = move(X, Y, Move),
                case sets:is_element({NX, NY}, VisitedAcc) of
                    true ->
                        Acc;
                    false ->
                        case run(Process#process{input = Move}) of
                            #process{output = ?STATUS_UNMOVED} ->
                                {QAcc, sets:add_element({NX, NY}, VisitedAcc)};
                            #process{output = ?STATUS_MOVED} = P ->
                                {queue:in({P, NX, NY, N + 1}, QAcc),
                                 sets:add_element({NX, NY}, VisitedAcc)}
                        end
                end
        end,
    {NewQ, NewVisited} = lists:foldl(F, {Q2, Visited}, ?MOVES),
    case queue:is_empty(NewQ) of
        true ->
            N;
        false ->
            bfs(NewQ, NewVisited)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
