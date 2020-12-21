-module(aoc_2020_21_2).

-export([start/0]).

-define(COUNTS, counts).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Inits = init(),
    {Map, _Ing} = run_impl(Inits),
    do(Map).

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

do(Map) ->
    do_impl(Map, []).

do_impl(Map, Res) ->
    case maps:size(Map) =:= length(Res) of
        true ->
            result(Res);
        _ ->
            F = fun(All, Ings, {MapAcc, ResAcc}) ->
                        case sets:size(Ings) of
                            1 ->
                                [Ing] = sets:to_list(Ings),
                                G = fun(GAll, _GIngs, GMapAcc) ->
                                            maps:update_with(GAll, fun(X) -> sets:del_element(Ing, X) end, GMapAcc)
                                    end,
                                {maps:fold(G, MapAcc, MapAcc),
                                 [{All, Ing} | ResAcc]};
                            _ ->
                                {MapAcc, ResAcc}
                        end
                end,
            {NewMap, NewRes} = maps:fold(F, {Map, Res}, Map),
            do_impl(NewMap, NewRes)
    end.

result(Res) ->
    F = fun({All1, _}, {All2, _}) -> All1 =< All2 end,
    L = lists:sort(F, Res),
    string:join([atom_to_list(X) || {_, X} <- L], ",").
