-module(aoc_2018_21_2).

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
    run([], 0, sets:new(), Code, IPReg).

-define(REG(X), ets:lookup_element(?TAB, X, 2)).

run(Lasts, IP, States, Code, IPReg) ->
    ets:insert(?TAB, {IPReg, IP}),
    {Op, A, B, C} = array:get(IP, Code),
    case execute(Lasts, States, Op, A, B, C) of
        loop ->
            unique(Lasts);
        {NewLasts, NewStates} ->
            run(NewLasts, ?REG(IPReg) + 1, NewStates, Code, IPReg)
    end.

execute(Lasts, States, Op, A, B, C) ->
    ets:insert(?TAB, {C, val(Op, A, B)}),
    detect(Lasts, States, Op, A, B, C).

val(addr, A, B) ->
    ?REG(A) + ?REG(B);
val(addi, A, B) ->
    ?REG(A) + B;
val(mulr, A, B) ->
    ?REG(A) * ?REG(B);
val(muli, A, B) ->
    ?REG(A) * B;
val(banr, A, B) ->
    ?REG(A) band ?REG(B);
val(bani, A, B) ->
    ?REG(A) band B;
val(borr, A, B) ->
    ?REG(A) bor ?REG(B);
val(bori, A, B) ->
    ?REG(A) bor B;
val(setr, A, _) ->
    ?REG(A);
val(seti, A, _) ->
    A;
val(gtir, A, B) ->
    case A > ?REG(B) of true -> 1; false -> 0 end;
val(gtri, A, B) ->
    case ?REG(A) > B of true -> 1; false -> 0 end;
val(gtrr, A, B) ->
    case ?REG(A) > ?REG(B) of true -> 1; false -> 0 end;
val(eqir, A, B) ->
    case A =:= ?REG(B) of true -> 1; false -> 0 end;
val(eqri, A, B) ->
    case ?REG(A) =:= B of true -> 1; false -> 0 end;
val(eqrr, A, B) ->
    case ?REG(A) =:= ?REG(B) of true -> 1; false -> 0 end.

detect(Lasts, States, eqrr, A, 0, _C) ->
    State = {?REG(0), ?REG(1), ?REG(2), ?REG(3), ?REG(4), ?REG(5)},
    case sets:is_element(State, States) of
        true ->
            loop;
        false ->
            {[?REG(A) | Lasts], sets:add_element(State, States)}
    end;
detect(Lasts, States, _, _, _, _) ->
    {Lasts, States}.

unique(Lasts) ->
    F = fun(X, {SeenAcc, DupsAcc}) ->
                case sets:is_element(X, SeenAcc) of
                    true ->
                        {SeenAcc, sets:add_element(X, DupsAcc)};
                    false ->
                        {sets:add_element(X, SeenAcc), DupsAcc}
                end
        end,
    {_, Dups} = lists:foldl(F, {sets:new(), sets:new()}, Lasts),
    hd(lists:dropwhile(fun(X) -> sets:is_element(X, Dups) end, Lasts)).
