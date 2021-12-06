-module(vm).

-export([start/0]).

-record(cpu, {ip = 0, regs, ram, stack = [], input = save(), debug = false}).

-define(NEWLINE, 10).

save() ->
    L = ["doorway",
         "north",
         "north",
         "bridge",
         "continue",
         "down",
         "east",
         "take empty lantern",
         "west",
         "west",
         "passage",
         "ladder",
         "west",
         "south",
         "north",
         "take can",
         "west",
         "use can",
         "use lantern",
         "east",
         "east",
         "south",
         "south",
         "ladder",
         "darkness",
         "continue",
         "west",
         "west",
         "west",
         "west",
         "north",
         "take red coin",
         "north",
         "east",
         "take concave coin",
         "down",
         "take corroded coin",
         "up",
         "west",
         "west",
         "take blue coin",
         "up",
         "take shiny coin",
         "down",
         "east",
         "use blue coin",
         "use red coin",
         "use shiny coin",
         "use concave coin",
         "use corroded coin",
         "north",
         "take teleporter",
         "use teleporter",
         "north",
         "north",
         "north",
         "north",
         "north",
         "north",
         "north",
         "north",
         "north",
         "take orb"
        ],
    lists:flatten([[X, ?NEWLINE] || X <- L]).

-define(MOD, 32768).
-define(MEMGET(X), array:get(X, RAM)).
-define(MEMSET(X, V), array:set(X, V, RAM)).
-define(REGSET(R, V), array:set(R - ?MOD, V, Regs)).
-define(REGGET(R), array:get(R - ?MOD, Regs)).
-define(VAL(X), case X < ?MOD of true -> X; false -> ?REGGET(X) end).

start() ->
    io:format("path: ~p~n", [bfs()]),
    CPU = load(init(), "challenge.bin"),
    run(CPU).

bfs() ->
    L = [[$*, 8, $-, 1],
         [4, $*, 11, $*],
         [$+, 4, $-, 18],
         [22, $-, 9, $*]],
    Graph = array:from_list([array:from_list(X) || X <- L]),
    bfs_impl(queue:from_list([{22, undefined, 0, 3, [22]}]), sets:new(), Graph).

-define(D, [{1, 0}, {0, 1}, {-1, 0}, {0, -1}]).

bfs_impl(Q, Seen, Graph) ->
    {{value, {Weight, Op, X, Y, Path}}, Q2} = queue:out(Q),
    Neighbors = [{X + DX, Y + DY} || {DX, DY} <- ?D],
    F = fun({NX, NY}, {QAcc, SeenAcc} = Acc) ->
                case NX >= 0 andalso NX < 4 andalso NY >= 0 andalso NY < 4
                    andalso (NX =/= 0 orelse NY =/= 3) of
                    false ->
                        Acc;
                    true ->
                        case a(NX, NY, Graph) of
                            O when O >= $* ->
                                {queue:in({Weight, O, NX, NY, [O | Path]}, QAcc), SeenAcc};
                            N ->
                                V = op(Op, Weight, N),
                                case {{NX, NY, V}, sets:is_element({NX, NY, V}, SeenAcc)} of
                                    {{3, 0, 30}, _} ->
                                        [N | Path];
                                    {{3, 0, _}, _} ->
                                        Acc;
                                    {_, true} ->
                                        Acc;
                                    {_, false} ->
                                        {queue:in({V, undefined, NX, NY, [N | Path]}, QAcc),
                                         sets:add_element({NX, NY, V}, SeenAcc)}
                                end
                        end
                end;
           (_, Acc) ->
                Acc
        end,
    case lists:foldl(F, {Q2, Seen}, Neighbors) of
        {NewQ, NewSeen} ->
            bfs_impl(NewQ, NewSeen, Graph);
        Res ->
            Res
    end.

a(X, Y, Arr) ->
    array:get(X, array:get(Y, Arr)).

op($+, A, B) ->
    A + B;
op($-, A, B) ->
    A - B;
op($*, A, B) ->
    A * B.

init() ->
    Regs = array:new(8, {default, 0}),
    RAM = array:new(1 bsl 15, {default, 0}),
    #cpu{regs = Regs, ram = RAM}.

load(#cpu{ram = RAM} = CPU, Filename) ->
    {ok, Binary} = file:read_file(Filename),
    CPU#cpu{ram = load_impl(0, Binary, RAM)}.

load_impl(_, <<>>, Arr) ->
    Arr;
load_impl(Addr, <<Low:8, High:8, T/binary>>, Arr) ->
    load_impl(Addr + 1, T, array:set(Addr, (High bsl 8) + Low, Arr)).

-define(OP_HALT, 0).
-define(OP_SET, 1).
-define(OP_PUSH, 2).
-define(OP_POP, 3).
-define(OP_EQ, 4).
-define(OP_GT, 5).
-define(OP_JMP, 6).
-define(OP_JT, 7).
-define(OP_JF, 8).
-define(OP_ADD, 9).
-define(OP_MULT, 10).
-define(OP_MOD, 11).
-define(OP_AND, 12).
-define(OP_OR, 13).
-define(OP_NOT, 14).
-define(OP_RMEM, 15).
-define(OP_WMEM, 16).
-define(OP_CALL, 17).
-define(OP_RET, 18).
-define(OP_OUT, 19).
-define(OP_IN, 20).
-define(OP_NOOP, 21).

run(#cpu{input = Input} = CPU) ->
    Instruction = instruction(CPU),
    Inputs = case Instruction of
                 {?OP_IN, _} when Input =:= [] ->
                     io:get_line("");
                 _ ->
                     Input
             end,
    case execute(Instruction, CPU#cpu{input = Inputs}) of
        halt ->
            ok;
        error ->
            error;
        CPU2 ->
            run(CPU2)
    end.

instruction(#cpu{ip = IP, ram = RAM}) ->
    Op = ?MEMGET(IP),
    case nargs(Op) of
        0 ->
            {Op, []};
        N ->
            {Op, [?MEMGET(IP + I) || I <- lists:seq(1, N)]}
    end.

nargs(?OP_HALT) -> 0;
nargs(?OP_SET) -> 2;
nargs(?OP_PUSH) -> 1;
nargs(?OP_POP) -> 1;
nargs(?OP_EQ) -> 3;
nargs(?OP_GT) -> 3;
nargs(?OP_JMP) -> 1;
nargs(?OP_JT) -> 2;
nargs(?OP_JF) -> 2;
nargs(?OP_ADD) -> 3;
nargs(?OP_MULT) -> 3;
nargs(?OP_MOD) -> 3;
nargs(?OP_AND) -> 3;
nargs(?OP_OR) -> 3;
nargs(?OP_NOT) -> 2;
nargs(?OP_RMEM) -> 2;
nargs(?OP_WMEM) -> 2;
nargs(?OP_CALL) -> 1;
nargs(?OP_RET) -> 0;
nargs(?OP_OUT) -> 1;
nargs(?OP_IN) -> 1;
nargs(?OP_NOOP) -> 0.

execute(Instruction, #cpu{debug = Debug} = CPU) ->
    Debug andalso debug(Instruction, CPU),
    execute_impl(Instruction, CPU).

execute_impl({?OP_HALT, []}, _) ->
    halt;
execute_impl({?OP_SET, [A, B]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    CPU#cpu{ip = IP + 3, regs = ?REGSET(A, ?VAL(B))};
execute_impl({?OP_PUSH, [A]}, #cpu{ip = IP, regs = Regs, stack = S} = CPU) ->
    CPU#cpu{ip = IP + 2, stack = [?VAL(A) | S]};
execute_impl({?OP_POP, [_]}, #cpu{stack = []}) ->
    error;
execute_impl({?OP_POP, [A]}, #cpu{ip = IP, regs = Regs, stack = [H|T]} = CPU) ->
    CPU#cpu{ip = IP + 2, regs = ?REGSET(A, H), stack = T};
execute_impl({?OP_EQ, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = case ?VAL(B) =:= ?VAL(C) of true -> 1; false -> 0 end,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_GT, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = case ?VAL(B) > ?VAL(C) of true -> 1; false -> 0 end,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_JMP, [A]}, #cpu{regs = Regs} = CPU) ->
    CPU#cpu{ip = ?VAL(A)};
execute_impl({?OP_JT, [A, B]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    Jmp = case ?VAL(A) of 0 -> IP + 3; _ -> ?VAL(B) end,
    CPU#cpu{ip = Jmp};
execute_impl({?OP_JF, [16#8007, _]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    io:format("### hacking r7 to..."),
    R7 = ackermann(),
    io:format("~p~n", [R7]),
    CPU#cpu{ip = IP + 3, regs = ?REGSET(16#8007, R7)};
execute_impl({?OP_JF, [A, B]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    Jmp = case ?VAL(A) of
              0 ->
                  ?VAL(B);
              _ ->
                  IP + 3
          end,
    CPU#cpu{ip = Jmp};
execute_impl({?OP_ADD, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = (?VAL(B) + ?VAL(C)) rem ?MOD,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_MULT, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = (?VAL(B) * ?VAL(C)) rem ?MOD,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_MOD, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = ?VAL(B) rem ?VAL(C),
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_AND, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = ?VAL(B) band ?VAL(C),
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_OR, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = ?VAL(B) bor ?VAL(C),
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
execute_impl({?OP_NOT, [A, B]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = (bnot ?VAL(B)) band ((1 bsl 15) - 1),
    CPU#cpu{ip = IP + 3, regs = ?REGSET(A, V)};
execute_impl({?OP_RMEM, [A, B]}, #cpu{ip = IP, regs = Regs, ram = RAM} = CPU) ->
    CPU#cpu{ip = IP + 3, regs = ?REGSET(A, ?MEMGET(?VAL(B)))};
execute_impl({?OP_WMEM, [A, B]}, #cpu{ip = IP, regs = Regs, ram = RAM} = CPU) ->
    CPU#cpu{ip = IP + 3, ram = ?MEMSET(?VAL(A), ?VAL(B))};
execute_impl({?OP_CALL, [6027]}, #cpu{ip = 5489, regs = Regs} = CPU) ->
    io:format("### bypassing confirmation check, setting r0 to 6~n"),
    CPU#cpu{ip = 5491, regs = ?REGSET(16#8000, 6)};
execute_impl({?OP_CALL, [A]}, #cpu{ip = IP, regs = Regs, stack = S} = CPU) ->
    CPU#cpu{ip = ?VAL(A), stack = [IP + 2 | S]};
execute_impl({?OP_RET, []}, #cpu{stack = []}) ->
    halt;
execute_impl({?OP_RET, []}, #cpu{regs = Regs, stack = [H | T]} = CPU) ->
    CPU#cpu{ip = ?VAL(H), stack = T};
execute_impl({?OP_OUT, [A]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    io:format("~c", [?VAL(A)]),
    CPU#cpu{ip = IP + 2};
execute_impl({?OP_IN, [A]}, #cpu{ip = IP, regs = Regs, input = [H | T]} = CPU) ->
    CPU#cpu{ip = IP + 2, regs = ?REGSET(A, H), input = T};
execute_impl({?OP_NOOP, []}, #cpu{ip = IP} = CPU) ->
    CPU#cpu{ip = IP + 1}.

%% Modified Ackermann function!
%% A(0, n)         = n + 1
%% A(m + 1, 0)     = A(m, x)
%% A(m + 1, n + 1) = A(m, A(m + 1, n))
%% This function is to find x where A(4, 1) = 6 (mod ?MOD)
%% m\n    0           1          2      ...                n
%% --+------------------------------------------------------------------------
%% 0 |    1           2          3                        n+1
%% 1 |   x+1         x+2        x+3                      n+x+1
%% 2 |  2x+1        3x+2       4x+3                     x^2+3x+1
%% 3 | x^2+3x+1      ...        ...      (x+1)^n(x^2+3x+1) + ((x+1)^n-1)(2x+1)/x
%% 4 | A(3,x)   A(3, A(3,x))
ackermann() ->
    25734.
%%     ackermann_impl(0).

%% ackermann_impl(X) ->
%%     A3x = ackermann3(X, X),
%%     case ackermann3(X, A3x) of
%%         6 ->
%%             X;
%%         _ ->
%%             ackermann_impl(X + 1)
%%     end.

%% %% (x+1)^n(x^2+3x+1) + (2x+1)(1+(x+1)+(x+1)^2+...+(x+1)^(n-1))
%% ackermann3(X, 0) ->
%%     (X * X + 3 * X + 1) rem ?MOD;
%% ackermann3(X, N) ->
%%     Pow = pow(X + 1, N),
%%     PowSum = pow_sum(X + 1, N),
%%     (Pow * (X * X + 3 * X + 1) + (2 * X + 1) * PowSum) rem ?MOD.

%% pow(X, N) ->
%%     pow_impl(1, X, N).

%% pow_impl(Acc, _, 0) ->
%%     Acc;
%% pow_impl(Acc, X, N) when N band 1 =:= 0 ->
%%     pow_impl(Acc, X * X rem ?MOD, N div 2);
%% pow_impl(Acc, X, N) when N band 1 =/= 0 ->
%%     pow_impl(Acc * X rem ?MOD, X * X rem ?MOD, N div 2).

%% pow_sum(X, N) ->
%%     pow_sum_impl(0, 1, N, X).

%% pow_sum_impl(Acc, _, 0, _) ->
%%     Acc;
%% pow_sum_impl(Acc, Pow, N, X) ->
%%     pow_sum_impl((Acc + Pow) rem ?MOD, Pow * X rem ?MOD, N - 1, X).

debug(Instruction, #cpu{ip = IP, regs = Regs, stack = Stack} = CPU) ->
    io:format("~n"),
    io:format("[~p]: ~p~n", [IP, Instruction]),
    io:format("regs: ~p~n", [array:to_list(Regs)]),
    io:format("stack: ~p~n", [Stack]),
    debug_impl(Instruction, CPU).

debug_impl({?OP_HALT, []}, _) ->
    io:format("halting");
debug_impl({?OP_SET, [A, B]}, #cpu{regs = Regs}) ->
    io:format("setting reg ~p to ~p(~p)~n", [A - ?MOD, B, ?VAL(B)]);
debug_impl({?OP_PUSH, [A]}, #cpu{regs = Regs, stack = S}) ->
    io:format("pushing ~p(~p) to stack: ~p~n", [A, ?VAL(A), S]);
debug_impl({?OP_POP, [_]}, #cpu{stack = []}) ->
    error;
debug_impl({?OP_POP, [A]}, #cpu{ip = IP, regs = Regs, stack = [H|T]} = CPU) ->
    CPU#cpu{ip = IP + 2, regs = ?REGSET(A, H), stack = T};
debug_impl({?OP_EQ, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = case ?VAL(B) =:= ?VAL(C) of true -> 1; false -> 0 end,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
debug_impl({?OP_GT, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = case ?VAL(B) > ?VAL(C) of true -> 1; false -> 0 end,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
debug_impl({?OP_JMP, [A]}, #cpu{regs = Regs}) ->
    io:format("jumping to ~p(~p)~n", [A, ?VAL(A)]);
debug_impl({?OP_JT, [A, B]}, #cpu{regs = Regs}) ->
    io:format("jumping to ~p(~p) if ~p(~p) isn't 0~n", [B, ?VAL(B), A, ?VAL(A)]);
debug_impl({?OP_JF, [A, B]}, #cpu{regs = Regs}) ->
    io:format("jumping to ~p(~p) if ~p(~p) is 0~n", [B, ?VAL(B), A, ?VAL(A)]);
debug_impl({?OP_ADD, [A, B, C]}, #cpu{regs = Regs}) ->
    io:format("adding ~p(~p) and ~p(~p) to reg ~p~n",
              [B, ?VAL(B), C, ?VAL(C), A - ?MOD]);
debug_impl({?OP_MULT, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = (?VAL(B) * ?VAL(C)) rem ?MOD,
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
debug_impl({?OP_MOD, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = ?VAL(B) rem ?VAL(C),
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
debug_impl({?OP_AND, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = ?VAL(B) band ?VAL(C),
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
debug_impl({?OP_OR, [A, B, C]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = ?VAL(B) bor ?VAL(C),
    CPU#cpu{ip = IP + 4, regs = ?REGSET(A, V)};
debug_impl({?OP_NOT, [A, B]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    V = (bnot ?VAL(B)) band ((1 bsl 15) - 1),
    CPU#cpu{ip = IP + 3, regs = ?REGSET(A, V)};
debug_impl({?OP_RMEM, [A, B]}, #cpu{ip = IP, regs = Regs, ram = RAM} = CPU) ->
    CPU#cpu{ip = IP + 3, regs = ?REGSET(A, ?MEMGET(?VAL(B)))};
debug_impl({?OP_WMEM, [A, B]}, #cpu{ip = IP, regs = Regs, ram = RAM} = CPU) ->
    CPU#cpu{ip = IP + 3, ram = ?MEMSET(?VAL(A), ?VAL(B))};
debug_impl({?OP_CALL, [A]}, #cpu{regs = Regs}) ->
    io:format("calling ~p(~p)~n", [A, ?VAL(A)]);
debug_impl({?OP_RET, []}, #cpu{stack = []}) ->
    halt;
debug_impl({?OP_RET, []}, #cpu{regs = Regs, stack = [H | T]} = CPU) ->
    CPU#cpu{ip = ?VAL(H), stack = T};
debug_impl({?OP_OUT, [A]}, #cpu{ip = IP, regs = Regs} = CPU) ->
    io:format("~c", [?VAL(A)]),
    CPU#cpu{ip = IP + 2};
debug_impl({?OP_IN, [A]}, #cpu{ip = IP, regs = Regs, input = [H | T]} = CPU) ->
    CPU#cpu{ip = IP + 2, regs = ?REGSET(A, H), input = T};
debug_impl({?OP_NOOP, []}, #cpu{ip = IP} = CPU) ->
    CPU#cpu{ip = IP + 1}.
