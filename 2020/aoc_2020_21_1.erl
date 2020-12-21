-module(aoc_2020_21_1).

-export([start/0]).

-define(COUNTS, counts).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Inits = init(),
    {Map, Ing} = run_impl(Inits),
    do(Map, Ing).

init() ->
    ets:new(?COUNTS, [named_table]),
    %% all -> set of possible ings
    Map = maps:new(),
    Ing = sets:new(),
    {Map, Ing}.

run_impl({Map, Ing} = Inits) ->
    case io:get_line("") of
        eof ->
            Inits;
        L ->
            {Ings, Alls} = parse(L),
            sets:fold(fun(X, _) -> ets:update_counter(?COUNTS, X, 1, {X, 0}) end,
                      undefined,
                      Ings),
            NewIng = sets:union(Ing, Ings),
            F = fun(All, Acc) ->
                        maps:update_with(All,
                                         fun(X) -> sets:intersection(X, Ings) end,
                                         Ings,
                                         Acc)
                end,
            NewMap = lists:foldl(F, Map, Alls),
            run_impl({NewMap, NewIng})
    end.

parse(L) ->
    L2 = string:trim(L, trailing, ")\n"),
    [P1s, P2s] = string:split(L2, "("),
    IngStrs = string:split(string:trim(P1s, trailing), " ", all),
    Ings = sets:from_list([list_to_atom(S) || S <- IngStrs]),
    [_ | AllStrs] = string:split(P2s, " ", all),
    Alls = [list_to_atom(string:trim(S, trailing, ",")) || S <- AllStrs],
    {Ings, Alls}.

do(Map, Ing) ->
    F = fun(_All, Ings, IngAcc) ->
                sets:subtract(IngAcc, Ings)
        end,
    Impossibles = maps:fold(F, Ing, Map),
    G = fun(X, Acc) ->
                Acc + ets:lookup_element(?COUNTS, X, 2)
        end,
    sets:fold(G, 0, Impossibles).
