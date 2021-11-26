-module(aoc_2019_11_2).

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

-record(process, {output = undefined, ip, code, x, y, dx, dy, whites}).

do(X) ->
    run(#process{ip = 0, code = X, x = 0, y = 0, dx = 0, dy = 1,
                 whites = sets:from_list([{0, 0}])}).

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
        {?OP_HALT, []} ->
            print(Process),
            0;
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

-define(SPACE, 16#20).

print(#process{whites = Whites}) ->
    {Xs, Ys} = lists:unzip(sets:to_list(Whites)),
    {MinX, MaxX} = bounds(Xs),
    {MinY, MaxY} = bounds(Ys),
    Seq = lists:seq(MinX, MaxX),
    F = fun(Y) ->
                L = [case sets:is_element({X, Y}, Whites) of
                         true ->
                             $.;
                         false ->
                             ?SPACE
                     end || X <- Seq],
                io:format("~s~n", [L])
        end,
    lists:foreach(F, lists:seq(MaxY, MinY, -1)).

bounds(L) ->
    Sorted = lists:sort(L),
    {hd(Sorted), lists:last(Sorted)}.

-define(COLOR, color(Process)).

color(#process{x = X, y = Y, whites = Whites}) ->
    case sets:is_element({X, Y}, Whites) of
        true ->
            1;
        false ->
            0
    end.

execute({?OP_ADD, [PA, PB, PC]}, #process{code = Code, ip = IP} = Process) ->
    V = val(PA, Code) + val(PB, Code),
    Process#process{ip = IP + 4, code = array:set(addr(PC), V, Code)};
execute({?OP_MUL, [PA, PB, PC]}, #process{code = Code, ip = IP} = Process) ->
    V = val(PA, Code) * val(PB, Code),
    Process#process{ip = IP + 4, code = array:set(addr(PC), V, Code)};
execute({?OP_IN, [P]}, #process{code = Code, ip = IP} = Process) ->
    Process#process{ip = IP + 2, code = array:set(addr(P), ?COLOR, Code)};
execute({?OP_OUT, [P]}, #process{ip = IP, code = Code} = Process) ->
    Output = val(P, Code),
    Process2 = process_output(Output, Process),
    Process2#process{ip = IP + 2};
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

process_output(Output, #process{output = undefined} = Process) ->
    Process#process{output = Output};
process_output(Output, Process) ->
    Process2 = paint(Process),
    Process3 = turn(Output, Process2),
    step(Process3).

paint(#process{output = 0, x = X, y = Y, whites = Whites} = Process) ->
    Process#process{output = undefined,
                    whites = sets:del_element({X, Y}, Whites)};
paint(#process{output = 1, x = X, y = Y, whites = Whites} = Process) ->
    Process#process{output = undefined,
                    whites = sets:add_element({X, Y}, Whites)}.

turn(Output, #process{dx = DX, dy = DY} = Process) ->
    {NDX, NDY} = turn_impl(Output, DX, DY),
    Process#process{dx = NDX, dy = NDY}.

%% left
turn_impl(0, 0, 1) ->
    {-1, 0};
turn_impl(0, -1, 0) ->
    {0, -1};
turn_impl(0, 0, -1) ->
    {1, 0};
turn_impl(0, 1, 0) ->
    {0, 1};
%% right
turn_impl(1, 0, 1) ->
    {1, 0};
turn_impl(1, 1, 0) ->
    {0, -1};
turn_impl(1, 0, -1) ->
    {-1, 0};
turn_impl(1, -1, 0) ->
    {0, 1}.

step(#process{x = X, y = Y, dx = DX, dy = DY} = Process) ->
    Process#process{x = X + DX, y = Y + DY}.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
