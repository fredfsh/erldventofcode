-module(aoc_2019_21_2).

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
    case run(#process{input = script(), code = Code}) of
        halt ->
            run(#process{input = script2(), code = Code});
        Res ->
            Res
    end.

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

run(Process) ->
    case instruction(Process) of
        {?OP_HALT, _} ->
            halt;
        {?OP_OUT, _} = Instruction ->
            case execute(Instruction, Process) of
                #process{output = Output} when Output > 127 ->
                    Output;
                Process2 ->
                    run(Process2)
            end;
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
        #process{input = [H | T], ip = IP, rb = RB, code = Code} = Process) ->
    Process#process{input = T, ip = IP + 2, code = array:set(?ADDR(P), H, Code)};
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

%%  ABCDEFGHI
%% @.XXXXXXXX => Jump
%% ----------
%%  ABCDEFGHI
%% @#XX.XXXXX => Not Jump
%% ----------
%%  ABCDEFGHI
%% @#.X#XXXXX => Jump
%% ----------
%%  ABCDEFGHI
%% @####XXXXX => Not Jump
%% ----------
%%  ABCDEFGHI
%% @##.##XXXX => Jump
%% ----------
%%  ABCDEFGHI
%% @##.#..XXX => Jump
%% ----------
%%  ABCDEFGHI
%% @##.#.##XX => Not Jump
%% ----------
%%  ABCDEFGHI
%% @##.#.#..X => Not Jump
%% ----------
%%  ABCDEFGHI
%% @##.#.#.## => Jump
%% ----------
%%  ABCDEFGHI
%% @##.#.#.#. => ???
%%
%% Can't decide whether to jump here, so we try both
%%    ABCDEFGHI
%%   @##.#.#.#...#### => Jump    (script2)
%%   @##.#.#.#.#..### => No jump (script)
%%
%% J = ~A | A&~B&D | A&B&~C&D&E | A&B&~C&D&~E&~F | A&B&~C&D&~E&F&~G&H&I
%%
%% J =     ||                          (5)
%%       /    \
%%    ~A       || && D                 (4)
%%           /    \
%%        ~B       || && ~C            (3)
%%               /    \
%%             E       ||              (2)
%%                   /    \
%%                ~F   ~G && H && I    (1)
%%
%% T = NOT G
%% T = T AND H
%% T = T AND I  (1)
%% J = NOT F
%% T = T OR J   (2)
%% T = T OR E
%% J = NOT C
%% T = T AND J  (3)
%% J = NOT B
%% T = T OR J
%% T = T AND D  (4)
%% J = NOT A
%% J = J OR T   (5)
script() ->
    Code = ["NOT G T",
            "AND H T",
            "AND I T",  %% (1)
            "NOT F J",
            "OR J T",   %% (2)
            "OR E T",
            "NOT C J",
            "AND J T",  %% (3)
            "NOT B J",
            "OR J T",
            "AND D T",  %% (4)
            "NOT A J",
            "OR T J"],  %% (5)
    lists:flatten([[X, ?NEWLINE] || X <- Code] ++ ["RUN", ?NEWLINE]).

%% only difference is to ignore I
script2() ->
    Code = ["NOT G T",
            "AND H T",  %% (1)
            "NOT F J",
            "OR J T",   %% (2)
            "OR E T",
            "NOT C J",
            "AND J T",  %% (3)
            "NOT B J",
            "OR J T",
            "AND D T",  %% (4)
            "NOT A J",
            "OR T J"],  %% (5)
    lists:flatten([[X, ?NEWLINE] || X <- Code] ++ ["RUN", ?NEWLINE]).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
