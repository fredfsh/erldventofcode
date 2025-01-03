-module(aoc_2024_17_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~s~n", [Out]),
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

-define(FMT, "Register A: ~d Register B: ~d Register C: ~d Program: ~s").

-record(cpu, {ip = 0, a, b, c, out = [], code}).

input() ->
    case io:fread("", ?FMT) of
        eof ->
            eof;
        {ok, [A, B, C, Txt]} ->
            L = string:lexemes(Txt, ","),
            Prog = array:from_list([list_to_integer(S) || S <- L]),
            #cpu{a = A, b = B, c = C, code = Prog}
    end.

ini() ->
    "".

-define(ADV, 0).
-define(BXL, 1).
-define(BST, 2).
-define(JNZ, 3).
-define(BXC, 4).
-define(OUT, 5).
-define(BDV, 6).
-define(CDV, 7).
-define(HALT, halt).

do(#cpu{out = Out} = CPU) ->
    case instruction(CPU) of
        ?HALT ->
            L = [integer_to_list(D) || D <- Out],
            lists:join($,, L);
        Instruction ->
%%            io:format("cpu: ~p~ninstruction: ~p~n", [CPU, Instruction]),
            do(execute(Instruction, CPU))
    end.

-define(C(IP), array:get(IP, Code)).

instruction(#cpu{ip = IP, code = Code}) ->
    case IP >= array:size(Code) of
        true ->
            ?HALT;
        false ->
            {?C(IP), ?C(IP + 1)}
    end.

-define(COMBO, case Op of
                   4 ->
                       CPU#cpu.a;
                   5 ->
                       CPU#cpu.b;
                   6 ->
                       CPU#cpu.c;
                   _ ->
                       Op
               end).

execute({?ADV, Op}, #cpu{ip = IP, a = A} = CPU) ->
    CPU#cpu{ip = IP + 2, a = trunc(A / math:pow(2, ?COMBO))};
execute({?BXL, Op}, #cpu{ip = IP, b = B} = CPU) ->
    CPU#cpu{ip = IP + 2, b = B bxor Op};
execute({?BST, Op}, #cpu{ip = IP} = CPU) ->
    CPU#cpu{ip = IP + 2, b = ?COMBO rem 8};
execute({?JNZ, _Op}, #cpu{ip = IP, a = 0} = CPU) ->
    CPU#cpu{ip = IP + 2};
execute({?JNZ, Op}, CPU) ->
    CPU#cpu{ip = Op};
execute({?BXC, _Op}, #cpu{ip = IP, b = B, c = C} = CPU) ->
    CPU#cpu{ip = IP + 2, b = B bxor C};
execute({?OUT, Op}, #cpu{ip = IP, out = Out} = CPU) ->
    CPU#cpu{ip = IP + 2, out = Out ++ [?COMBO rem 8]};
execute({?BDV, Op}, #cpu{ip = IP, a = A} = CPU) ->
    CPU#cpu{ip = IP + 2, b = trunc(A / math:pow(2, ?COMBO))};
execute({?CDV, Op}, #cpu{ip = IP, a = A} = CPU) ->
    CPU#cpu{ip = IP + 2, c = trunc(A / math:pow(2, ?COMBO))}.

acc(Acc, X) ->
    Acc ++ X.

fin(X) ->
    X.
