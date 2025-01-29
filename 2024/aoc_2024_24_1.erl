-module(aoc_2024_24_1).

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

-define(L2A(X), list_to_atom(X)).

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            case string:lexemes(L, " :->\n") of
                [] ->
                    undefined;
                [Wire, Sig] ->
                    {?L2A(Wire), list_to_integer(Sig)};
                [Lin, Gate, Rin, Out] ->
                    {?L2A(Out), ?L2A(Gate), ?L2A(Lin), ?L2A(Rin)}
            end
    end.

-define(TAB, memo).

ini() ->
    ets:new(?TAB, [named_table]),
    maps:new().

do(X) ->
    X.

acc(Acc, undefined) ->
    Acc;
acc(Acc, {Wire, Sig}) ->
    ets:insert_new(?TAB, {Wire, Sig}),
    Acc;
acc(Acc, {Wire, _, _, _} = Gate) ->
    maps:put(Wire, Gate, Acc).

fin(X) ->
    zates(X).

zates(Gates) ->
    F = fun(Wire) ->
                dp(Wire, Gates),
                hd(atom_to_list(Wire)) =:= $z
        end,
    Zates = lists:filter(F, maps:keys(Gates)),
%%    io:format("~p~n", [Zates]),
    Sorted = lists:sort(Zates),
%%    io:format("~p~n", [Sorted]),
    Sigs = [ets:lookup_element(?TAB, Wire, 2) || Wire <- Sorted],
%%    io:format("~p~n", [Sigs]),
    G = fun(Sig, Acc) ->
               (Acc bsl 1) bor Sig
        end,
    lists:foldr(G, 0, Sigs).

dp(Wire, Gates) ->
    case ets:lookup_element(?TAB, Wire, 2, undefined) of
        undefined ->
            Res = dp_impl(Wire, Gates),
            ets:insert_new(?TAB, {Wire, Res}),
%%            io:format("~p = ~p~n", [Wire, Res]),
            Res;
        Res ->
            Res
    end.


dp_impl(Wire, Gates) ->
    {Wire, Gate, Lin, Rin} = maps:get(Wire, Gates),
    LV = dp(Lin, Gates),
    RV = dp(Rin, Gates),
    mix(Gate, LV, RV).

-define(AND, 'AND').
-define( OR,  'OR').
-define(XOR, 'XOR').

mix(?AND, A, B) -> A band B;
mix( ?OR, A, B) -> A  bor B;
mix(?XOR, A, B) -> A bxor B.
