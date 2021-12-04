-module(aoc_2019_17_2).

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
-record(state, {x, y, dx, dy, ox = 0, oy = 0, scaffolds = sets:new(), mode}).

do(Code) ->
    State = run(#process{code = Code}, #state{mode = scan}),
    Input = routine(State),
    Override = array:set(0, 2, Code),
    run(#process{input = Input, code = Override}, #state{mode = clean}).

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

run(Process, #state{mode = Mode} = State) ->
    case instruction(Process) of
        {?OP_HALT, _} ->
            State;
        {?OP_OUT, _} = Instruction ->
            case execute(Instruction, Process) of
                #process{output = Output} when Output > 127 ->
                    Output;
                #process{output = Output} = Process2 when Mode =:= scan ->
                    State2 = process_output(Output, State),
                    run(Process2, State2);
                Process2 ->
                    run(Process2, State)
            end;
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

routine(State) ->
    Routine = routine_impl([0], State),
    breakdown(Routine).

routine_impl([N | T]=R, #state{x=X, y=Y, dx=DX, dy=DY, scaffolds=S} = State) ->
    {NX, NY} = {X + DX, Y + DY},
    case sets:is_element({NX, NY}, S) of
        true ->
            routine_impl([N + 1 | T], State#state{x = NX, y = NY});
        false ->
            case turn(State) of
                undefined ->
                    lists:reverse(R);
                {Dir, NDX, NDY} when R =:= [0] ->
                    routine_impl([0, Dir], State#state{dx = NDX, dy = NDY});
                {Dir, NDX, NDY} ->
                    routine_impl([0, Dir | R], State#state{dx = NDX, dy = NDY})
            end
    end.

turn(#state{x = X, y = Y, dx = DX, dy = DY, scaffolds = Scaffolds}) ->
    {LDX, LDY} = turn_left(DX, DY),
    {RDX, RDY} = turn_right(DX, DY),
    case {sets:is_element({X + LDX, Y + LDY}, Scaffolds),
          sets:is_element({X + RDX, Y + RDY}, Scaffolds)} of
        {true, _} ->
            {l, LDX, LDY};
        {_, true} ->
            {r, RDX, RDY};
        _ ->
            undefined
    end.

turn_left( 0,  1) -> direction($>);
turn_left( 1,  0) -> direction($^);
turn_left( 0, -1) -> direction($<);
turn_left(-1,  0) -> direction($v).

turn_right( 0,  1) -> direction($<);
turn_right(-1,  0) -> direction($^);
turn_right( 0, -1) -> direction($>);
turn_right( 1,  0) -> direction($v).

breakdown(L) ->
    {Main, Funs} = dfs([], maps:new(), $A, L),
    format(Main, Funs).

-define(ROUTINE_LEN, 20).

dfs(Main, _, _, _) when length(Main) > ?ROUTINE_LEN ->
    undefined;
dfs(Main, Funs, _, []) ->
    {lists:reverse(Main), Funs};
dfs(Main, Funs, Key, L) ->
    {Routine, Rest} = longest_routine(L),
    loop(Routine, Rest, Main, Funs, Key).

longest_routine(L) ->
    longest_routine_impl([], -1, L).

longest_routine_impl(Routine, _, []) ->
    {lists:reverse(Routine), []};
longest_routine_impl(Routine, Len, [D, N | T] = L) ->
    %%      ,   D   ,   N
    Delta = 1 + 1 + 1 + lg(N),
    case Len + Delta > ?ROUTINE_LEN of
        true ->
            {lists:reverse(Routine), L};
        false ->
            longest_routine_impl([N, D | Routine], Len + Delta, T)
    end.

lg(N) when N < 10 ->
    1;
lg(N) ->
    1 + lg(N div 10).

-define(MAX_ROUTINE, $C).

loop([], _, _, _, _) ->
    undefined;
loop(Routine, Rest, Main, Funs, Key) ->
    case maps:is_key(Routine, Funs) of
        false when Key > ?MAX_ROUTINE ->
            loop_next(Routine, Rest, Main, Funs, Key);
        _ ->
            loop_impl(Routine, Rest, Main, Funs, Key)
    end.

loop_impl(Routine, Rest, Main, Funs, Key) ->
    {NewMain, NewFuns, NewKey} =
        case maps:get(Routine, Funs, undefined) of
            undefined when Main =:= [] ->
                {[Key], maps:put(Routine, Key, Funs), Key + 1};
            undefined ->
                {[Key, $, | Main], maps:put(Routine, Key, Funs), Key + 1};
            Fun when Main =:= [] ->
                {[Fun], Funs, Key};
            Fun ->
                {[Fun, $, | Main], Funs, Key}
        end,
    case dfs(NewMain, NewFuns, NewKey, Rest) of
        undefined ->
            loop_next(Routine, Rest, Main, Funs, Key);
        Res ->
            Res
    end.

loop_next(Routine, Rest, Main, Funs, Key) ->
    {NewRoutine, [D, N]} = lists:split(length(Routine) - 2, Routine),
    loop(NewRoutine, [D, N | Rest], Main, Funs, Key).

format(Main, Funs) ->
    F = fun({Routine, _}) -> format(Routine) end,
    lists:flatten([Main, ?NEWLINE,
                   lists:map(F, lists:keysort(2, maps:to_list(Funs))),
                   $n, ?NEWLINE]).

format(Routine) ->
    format_impl([], Routine).

format_impl(Acc, []) ->
    lists:reverse([?NEWLINE | Acc]);
format_impl([], [D | T]) ->
    format_impl([ch(D)], T);
format_impl(Acc, [N | T]) when is_integer(N) ->
    format_impl(lists:reverse(integer_to_list(N)) ++ [$,] ++ Acc, T);
format_impl(Acc, [D | T]) ->
    format_impl([ch(D), $, | Acc], T).

ch(l) -> $L;
ch(r) -> $R.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
