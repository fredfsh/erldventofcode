-module(aoc_2018_19_2).

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
    case io:fread("", "~a ~d ~d ~d") of
        eof ->
            eof;
        {ok, [Op, A, B, C]} ->
            {Op, A, B, C}
    end.

ini() ->
    {ok, [IPReg]} = io:fread("", "#ip ~d"),
    {IPReg, array:new()}.

do(X) ->
    X.

acc({IPReg, Code}, X) ->
    {IPReg, array:set(array:size(Code), X, Code)}.

-define(TAB, regs).

fin({IPReg, Code}) ->
    ets:new(?TAB, [named_table]),
    ets:insert_new(?TAB, [{R, 0} || R <- lists:seq(0, 5)]),
    ets:insert(?TAB, {0, 1}),
    run(0, Code, IPReg).

%% Observe the program starts from the second half that prepares a large number
%% X stored in register #2, then jump to the first half, which is a loop to
%% calculate sum of all factors of X.
run(1, _, _) ->
    sum(ets:lookup_element(?TAB, 2, 2));
run(IP, Code, IPReg) ->
    ets:insert(?TAB, {IPReg, IP}),
    case IP < 0 orelse IP >= array:size(Code) of
        true ->
            ets:lookup_element(?TAB, 0, 2);
        false ->
            {Op, A, B, C} = array:get(IP, Code),
            execute(Op, A, B, C),
            run(ets:lookup_element(?TAB, IPReg, 2) + 1, Code, IPReg)
    end.

sum(X) ->
    sum_impl(0, 1, X).

sum_impl(Acc, N, X) when N > X ->
    Acc;
sum_impl(Acc, N, X) when X rem N =:= 0 ->
    sum_impl(Acc + N, N + 1, X);
sum_impl(Acc, N, X) ->
    sum_impl(Acc, N + 1, X).

execute(Op, A, B, C) ->
    ets:insert(?TAB, {C, val(Op, A, B)}).

val(addr, A, B) ->
    ets:lookup_element(?TAB, A, 2) + ets:lookup_element(?TAB, B, 2);
val(addi, A, B) ->
    ets:lookup_element(?TAB, A, 2) + B;
val(mulr, A, B) ->
    ets:lookup_element(?TAB, A, 2) * ets:lookup_element(?TAB, B, 2);
val(muli, A, B) ->
    ets:lookup_element(?TAB, A, 2) * B;
val(banr, A, B) ->
    ets:lookup_element(?TAB, A, 2) band ets:lookup_element(?TAB, B, 2);
val(bani, A, B) ->
    ets:lookup_element(?TAB, A, 2) band B;
val(borr, A, B) ->
    ets:lookup_element(?TAB, A, 2) bor ets:lookup_element(?TAB, B, 2);
val(bori, A, B) ->
    ets:lookup_element(?TAB, A, 2) bor B;
val(setr, A, _) ->
    ets:lookup_element(?TAB, A, 2);
val(seti, A, _) ->
    A;
val(gtir, A, B) ->
    case A > ets:lookup_element(?TAB, B, 2) of true -> 1; false -> 0 end;
val(gtri, A, B) ->
    case ets:lookup_element(?TAB, A, 2) > B of true -> 1; false -> 0 end;
val(gtrr, A, B) ->
    case ets:lookup_element(?TAB, A, 2) > ets:lookup_element(?TAB, B, 2) of
        true ->
            1;
        false ->
            0
    end;
val(eqir, A, B) ->
    case A =:= ets:lookup_element(?TAB, B, 2) of true -> 1; false -> 0 end;
val(eqri, A, B) ->
    case ets:lookup_element(?TAB, A, 2) =:= B of true -> 1; false -> 0 end;
val(eqrr, A, B) ->
    case ets:lookup_element(?TAB, A, 2) =:= ets:lookup_element(?TAB, B, 2) of
        true ->
            1;
        false ->
            0
    end.
