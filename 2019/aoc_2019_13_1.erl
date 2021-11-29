-module(aoc_2019_13_1).

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

-define(TAB, sys).
-define(RELBASE, relative_base).

ini() ->
    ets:new(?TAB, [named_table]),
    ets:insert_new(?TAB, {?RELBASE, 0}),
    0.

-record(process, {output = [], ip = 0, code}).

do(X) ->
    run(#process{code = X}).

-define(OP_HALT, 99).
-define(OP_ADD, 1).
-define(OP_MUL, 2).
%% -define(OP_IN, 3).
-define(OP_OUT, 4).
-define(OP_JTR, 5).
-define(OP_JFS, 6).
-define(OP_LT, 7).
-define(OP_EQ, 8).
-define(OP_REL, 9).

run(#process{output = Output} = Process) ->
    case instruction(Process) of
        {?OP_HALT, []} ->
            blocks(Output);
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
%% nparameters(?OP_IN) -> 1;
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

blocks(Output) ->
    blocks_impl(0, Output).

-define(TILE_BLOCK, 2).

blocks_impl(Acc, []) ->
    Acc;
blocks_impl(Acc, [?TILE_BLOCK, _, _ | T]) ->
    blocks_impl(Acc + 1, T);
blocks_impl(Acc, [_, _, _ | T]) ->
    blocks_impl(Acc, T).

execute({?OP_ADD, [PA, PB, PC]}, #process{code = Code, ip = IP} = Process) ->
    V = val(PA, Code) + val(PB, Code),
    Process#process{ip = IP + 4, code = array:set(addr(PC), V, Code)};
execute({?OP_MUL, [PA, PB, PC]}, #process{code = Code, ip = IP} = Process) ->
    V = val(PA, Code) * val(PB, Code),
    Process#process{ip = IP + 4, code = array:set(addr(PC), V, Code)};
%% execute({?OP_IN, [P]}, #process{code = Code, ip = IP} = Process) ->
%%     Process#process{ip = IP + 2, code = array:set(addr(P), ?INPUT, Code)};
execute({?OP_OUT, [P]}, #process{output = O, ip = IP, code = Code} = Process) ->
    Process#process{ip = IP + 2, output = [val(P, Code) | O]};
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
execute({?OP_LT, [PA, PB, PC]}, #process{code = Code, ip = IP} = Process) ->
    V = case val(PA, Code) < val(PB, Code) of
            true ->
                1;
            false ->
                0
        end,
    Process#process{ip = IP + 4, code = array:set(addr(PC), V, Code)};
execute({?OP_EQ, [PA, PB, PC]}, #process{code = Code, ip = IP} = Process) ->
    V = case val(PA, Code) =:= val(PB, Code) of
            true ->
                1;
            false ->
                0
        end,
    Process#process{ip = IP + 4, code = array:set(addr(PC), V, Code)};
execute({?OP_REL, [P]}, #process{code = Code, ip = IP} = Process) ->
    ets:update_counter(?TAB, ?RELBASE, val(P, Code)),
    Process#process{ip = IP + 2}.

-define(MODE_POSITION, 0).
-define(MODE_IMMEDIATE, 1).
-define(MODE_RELATIVE, 2).

val({Parameter, ?MODE_IMMEDIATE}, _) ->
    Parameter;
val(P, Code) ->
    ?V(addr(P)).

addr({Parameter, ?MODE_POSITION}) ->
    Parameter;
addr({Parameter, ?MODE_RELATIVE}) ->
    ets:lookup_element(?TAB, ?RELBASE, 2) + Parameter.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
