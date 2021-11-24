-module(aoc_2019_5_1).

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

do(X) ->
    run(0, 0, X).

-define(OP_HALT, 99).
-define(OP_ADD, 1).
-define(OP_MUL, 2).
-define(OP_IN, 3).
-define(OP_OUT, 4).

run(Output, IP, Code) ->
    case instruction(IP, Code) of
        {?OP_HALT, []} ->
            Output;
        {_, Parameters} = Instruction ->
            NewIP = IP + length(Parameters) + 1,
            case execute(Instruction, Code) of
                {NewCode, NewOutput} ->
                    run(NewOutput, NewIP, NewCode);
                NewCode ->
                    run(Output, NewIP, NewCode)
            end
    end.

-define(V(X), array:get(X, Code)).

instruction(IP, Code) ->
    Op = ?V(IP) rem 100,
    {Op, parameters(IP, Code, nparameters(Op))}.

nparameters(?OP_HALT) -> 0;
nparameters(?OP_ADD) -> 3;
nparameters(?OP_MUL) -> 3;
nparameters(?OP_IN) -> 1;
nparameters(?OP_OUT) -> 1.

parameters(IP, Code, P) ->
    parameters_impl([], ?V(IP) div 100, IP + 1, P, Code).

parameters_impl(Acc, _, _, 0, _) ->
    lists:reverse(Acc);
parameters_impl(Acc, Modes, Pos, P, Code) ->
    Mode = Modes rem 10,
    parameters_impl([{?V(Pos), Mode} | Acc], Modes div 10, Pos+1, P-1, Code).

-define(INPUT, 1).

execute({?OP_ADD, [PA, PB, {C, _}]}, Code) ->
    array:set(C, val(PA, Code) + val(PB, Code), Code);
execute({?OP_MUL, [PA, PB, {C, _}]}, Code) ->
    array:set(C, val(PA, Code) * val(PB, Code), Code);
execute({?OP_IN, [{P, _}]}, Code) ->
    array:set(P, ?INPUT, Code);
execute({?OP_OUT, [P]}, Code) ->
    {Code, val(P, Code)}.

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
