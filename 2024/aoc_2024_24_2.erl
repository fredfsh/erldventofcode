-module(aoc_2024_24_2).

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

-define(L2A(X), list_to_atom(X)).

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            case string:lexemes(L, " :->\n") of
                [] ->
                    undefined;
                [_, _] ->
                    undefined;
                [Lin, Gate, Rin, Out] ->
                    {?L2A(Out), ?L2A(Gate), ?L2A(Lin), ?L2A(Rin)}
            end
    end.

-define(TAB, gates).
-define(DEP, dependency).
-define(IDX, index).

ini() ->
    ets:new(?TAB, [named_table]),
    ets:new(?DEP, [named_table]),
    ets:new(?IDX, [bag, named_table]),
    ok.

do(X) ->
    X.

acc(Acc, undefined) ->
    Acc;
acc(Acc, Gate) ->
    ets:insert_new(?TAB, Gate),
    Acc.

fin(_) ->
    rebuild_deps(),
%%    io:format("~p~n", [ets:tab2list(?TAB)]),
%%    io:format("~p~n", [ets:tab2list(?DEP)]),
%%    io:format("~p~n", [ets:tab2list(?IDX)]),
    mispairs().

rebuild_deps() ->
    ets:delete_all_objects(?DEP),
    ets:delete_all_objects(?IDX),
    F = fun({Wire, _, _, _}, _) ->
                rebuild_dep(Wire)
        end,
    ets:foldl(F, undefined, ?TAB).

rebuild_dep(Wire) ->
    case {atom_to_list(Wire), ets:lookup_element(?DEP, Wire, 2, undefined)} of
        {[$x, _, _], undefined} ->
            [Wire];
        {[$y, _, _], undefined} ->
            [Wire];
        {_, undefined} ->
            [{Wire, _, Lin, Rin}] = ets:lookup(?TAB, Wire),
            LDeps = rebuild_dep(Lin),
            RDeps = rebuild_dep(Rin),
            Res = lists:umerge(LDeps, RDeps),
            ets:insert_new(?DEP, {Wire, Res}),
            ets:insert(?IDX, {Res, Wire}),
            Res;
        {_, Res} ->
            Res
    end.

%% A proper ADU must satisfy the following for each bit:
%% 1/ Inputs are properly wired. e.g. xAB wired to yAB
%% 2/ Proper output for arbitrary combination of corresponding input x bit,
%%    corresponding input y bit, and corresponding carry bit. (2x2x2 = 8/bit)
mispairs() ->
    inputs_properly_wired(),
    {Acc, Cire} = properly_wired([], undefined, 0, [0, 1], [0, 1], [0]),
    mispairs_impl(Acc, Cire, 1).

inputs_properly_wired() ->
    F = fun({_, _, Lin, Rin}, Acc) ->
                case {atom_to_list(Lin), atom_to_list(Rin)} of
                    {[$x, A, B], [$y, A, B]} ->
                        Acc;
                    {[$x, _, _], [_, _, _]} ->
                        false;
                    {[$y, A, B], [$x, A, B]} ->
                        Acc;
                    {[$y, _, _], [_, _, _]} ->
                        false;
                    {_, _} ->
                        Acc
                end
        end,
    true = ets:foldl(F, true, ?TAB).

%% fixes mismatched wires for this bit, if any, and returns the carry bit wire
properly_wired(Acc, Cire, Bit, Xs, Ys, Carries) ->
%%    io:format("properly wiring bit: ~p~n", [Bit]),
    Inputs = [{X, Y, C} || X <- Xs, Y <- Ys, C <- Carries],

    Zire = zire(Bit),
    OutputData = [{X, Y, C, (X + Y + C) band 1} || {X, Y, C} <- Inputs],
%%    io:format("determing output wire for bit: ~p~n", [Bit]),
    NewAcc = case wire(Bit, OutputData, Cire) of
                 undefined ->
%%                     io:format("output wire not found, fixing~n"),
                     fix_inner(Acc, Bit, OutputData, Cire, Zire);
                 Wire ->
%%                     io:format("output wire: ~p~n", [Wire]),
                     maybe_swap(Acc, Zire, Wire)
             end,

    CarryData = [{X, Y, C, (X + Y + C) bsr 1} || {X, Y, C} <- Inputs],
%%    io:format("determing carry wire for bit: ~p~n", [Bit]),
    NewCire = wire(Bit, CarryData, Cire),

    {NewAcc, NewCire}.

zire(Bit) -> ire($z, Bit).

ire(Prefix, Bit) when Bit < 10 ->
    list_to_atom([Prefix, $0, Bit + $0]);
ire(Prefix, Bit) ->
    list_to_atom([Prefix, Bit div 10 + $0, Bit rem 10 + $0]).

wire(Bit, Data, Cire) ->
    Key = key(Bit),
    Wires = ets:lookup_element(?IDX, Key, 2),
    F = fun(Wire) ->
                validate(Bit, Data, Cire, Wire)
        end,
    case lists:filter(F, Wires) of
        [Res] ->
            Res;
        [] ->
            undefined
    end.

key(Bit) ->
    Seq = lists:seq(0, Bit),
    Xeys = [xire(S) || S <- Seq],
    Yeys = [yire(S) || S <- Seq],
    Xeys ++ Yeys.

xire(Bit) -> ire($x, Bit).

yire(Bit) -> ire($y, Bit).

fix_inner(Acc, Bit, Data, Cire, Zire) ->
    FullKey = key(Bit),
    FullKeyWires = ets:lookup_element(?IDX, FullKey, 2),
    PartKey = [xire(Bit), yire(Bit)],
    PartKeyWires = ets:lookup_element(?IDX, PartKey, 2),
    F = fun(Wire) -> Wire < z end,
    Wires = lists:filter(F, FullKeyWires ++ PartKeyWires),
    Combs = [{L, R} || L <- Wires, R <- Wires, L < R],
    G = fun({Lire, Rire}) ->
                case dep(Lire, Rire) of
                    true ->
                        false;
                    false ->
                        swap(Lire, Rire),
                        Res = validate(Bit, Data, Cire, Zire),
                        swap(Lire, Rire),
                        Res
                end
        end,
    [{Lire, Rire}] = lists:filter(G, Combs),
    swap(Lire, Rire),
    [Lire, Rire | Acc].

dep(Lire, Rire) ->
    dep_impl(Lire, Rire) orelse dep_impl(Rire, Lire).

dep_impl(Lire, Rire) ->
    case ets:lookup(?TAB, Lire) of
        [] ->
            false;
        [{Lire, _, Rire, _}] ->
            true;
        [{Lire, _, _, Rire}] ->
            true;
        [{Lire, _, Lin, Rin}] ->
            dep_impl(Lin, Rire) orelse dep_impl(Rin, Rire)
    end.

maybe_swap(Acc, Wire, Wire) ->
    Acc;
maybe_swap(Acc, Lire, Rire) ->
    swap(Lire, Rire),
    [Lire, Rire | Acc].

swap(Lire, Rire) ->
%%    io:format("swapping: ~p, ~p~n", [Lire, Rire]),
    [Late] = ets:lookup(?TAB, Lire),
    [Rate] = ets:lookup(?TAB, Rire),
    ets:delete(?TAB, Lire),
    ets:delete(?TAB, Rire),
    ets:insert_new(?TAB, [setelement(1,Late,Rire), setelement(1,Rate,Lire)]),
    rebuild_deps().

validate(Bit, Data, Cire, Wire) ->
    F = fun({X, Y, C, W}) ->
                validate_impl(Bit, X, Y, C, Cire, W, Wire)
        end,
    lists:all(F, Data).

validate_impl(Bit, X, Y, C, Cire, W, Wire) ->
%%    io:format("validating ~p~n", [{Bit, X, Y, C, Cire, W, Wire}]),
    Xire = xire(Bit),
    Yire = yire(Bit),
    [{Wire, Gate, Lin, Rin}] = ets:lookup(?TAB, Wire),
    LV = sig(Lin, X, Xire, Y, Yire, C, Cire),
    RV = sig(Rin, X, Xire, Y, Yire, C, Cire),
    mix(Gate, LV, RV) =:= W.

sig(Xire, X, Xire, _, _, _, _) ->
    X;
sig(Yire, _, _, Y, Yire, _, _) ->
    Y;
sig(Cire, _, _, _, _, C, Cire) ->
    C;
sig(Wire, X, Xire, Y, Yire, C, Cire) ->
    [{Wire, Gate, Lin, Rin}] = ets:lookup(?TAB, Wire),
    LV = sig(Lin, X, Xire, Y, Yire, C, Cire),
    RV = sig(Rin, X, Xire, Y, Yire, C, Cire),
    mix(Gate, LV, RV).

-define(AND, 'AND').
-define( OR,  'OR').
-define(XOR, 'XOR').

mix(?AND, A, B) -> A band B;
mix( ?OR, A, B) -> A  bor B;
mix(?XOR, A, B) -> A bxor B.

mispairs_impl(Acc, Cire, Bit) ->
    case length(Acc) of
        8 ->
            L1 = lists:sort(Acc),
            L2 = [atom_to_list(X) || X <- L1],
            L3 = lists:join($,, L2),
            L4 = lists:flatten(L3),
            L4;
        _ ->
            {NewAcc, NewCire} =
                properly_wired(Acc, Cire, Bit, [0, 1], [0, 1], [0, 1]),
            mispairs_impl(NewAcc, NewCire, Bit + 1)
    end.
