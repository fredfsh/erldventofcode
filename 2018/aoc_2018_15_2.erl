-module(aoc_2018_15_2).

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
            X
    end.

ini() ->
    {0, sets:new(), []}.

do(X) ->
    X.

-define(HP, 200).

acc({Y, Map, Units}, L) ->
    F = fun($#, {X, MapAcc, UnitsAcc}) ->
                {X + 1, MapAcc, UnitsAcc};
           ($., {X, MapAcc, UnitsAcc}) ->
                {X + 1, sets:add_element({Y, X}, MapAcc), UnitsAcc};
           (Symbol, {X, MapAcc, UnitsAcc}) ->
                Faction = case Symbol of $G -> goblin; $E -> elf end,
                {X + 1, MapAcc, [{{Y, X}, {?HP, Faction}} | UnitsAcc]}
        end,
    {_, NewMap, NewUnits} = lists:foldl(F, {0, Map, Units}, L),
    {Y + 1, NewMap, NewUnits}.

fin({_, Map, Units}) ->
    Elves = elves(Units),
    bsearch(3, 200, Map, Units, Elves).

elves(Units) ->
    lists:sum([case Faction of elf -> 1; goblin -> 0 end
               || {_, {_, Faction}} <- Units]).

bsearch(Left, Right, Map, Units, Elves) ->
    case Left + 1 of
        Right ->
            {Res, _} = sim(0, Map, Units, Right),
            Res;
        _ ->
            Mid = Left + (Right - Left) div 2,
            case sim(0, Map, Units, Mid) of
                {_, Elves} ->
                    bsearch(Left, Mid, Map, Units, Elves);
                _ ->
                    bsearch(Mid, Right, Map, Units, Elves)
            end
    end.

sim(Rounds, Map, Units, Attack) ->
    Creeps = lists:keysort(1, Units),
    turn([], Rounds, Map, Creeps, Attack).

turn(Acc, Rounds, Map, [], Attack) ->
    sim(Rounds + 1, Map, Acc, Attack);
turn(Acc, Rounds, Map, [{{UY, UX}, {_, Faction}} = Unit | T], Attack) ->
    case targets(Faction, T, Acc) of
        [] ->
            {Rounds * hp(Acc, [Unit | T]), elves(Acc) + elves([Unit | T])};
        Targets ->
            {NewMap, NewUnit} =
                case chosen(Targets, UY, UX, Map) of
                    {UY, UX} ->
                        {Map, Unit};
                    undefined ->
                        {Map, Unit};
                    {Y, X} ->
                        move(Map, Unit, Y, X)
                end,
            {NMap, NAcc, NT} = attack(NewUnit, NewMap, Acc, T, Targets, Attack),
            turn([NewUnit | NAcc], Rounds, NMap, NT, Attack)
    end.

targets(Faction, L1, L2) ->
    F = fun({_, {_, Tribe}}) -> Tribe =/= Faction end,
    lists:append(lists:filter(F, L1), lists:filter(F, L2)).

hp(L1, L2) ->
    hp(L1) + hp(L2).

hp(L) ->
    lists:sum([HP || {_, {HP, _}} <- L]).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

chosen(Targets, UY, UX, Map) ->
    F = fun(_, {_, _} = Chosen) ->
                Chosen;
           ({{TY, TX}, _}, FAcc) ->
                G = fun(_, {_, _} = Chosen) ->
                            Chosen;
                       ({DY, DX}, GAcc) ->
                            case {TY + DY, TX + DX} of
                                {UY, UX} ->
                                    {UY, UX};
                                YX ->
                                    case sets:is_element(YX, Map) of
                                        true ->
                                            sets:add_element(YX, GAcc);
                                        false ->
                                            GAcc
                                    end
                            end
                    end,
                lists:foldl(G, FAcc, ?D)
        end,
    case lists:foldl(F, sets:new(), Targets) of
        {_, _} = Chosen ->
            Chosen;
        Candidates ->
            case reach(UY, UX, Candidates, Map) of
                [] ->
                    undefined;
                Reachables ->
                    {_, Y, X} = hd(lists:sort(Reachables)),
                    {Y, X}
            end
    end.

reach(UY, UX, Candidates, Map) ->
    Q = queue:from_list([{UY, UX, 0}]),
    bfs([], Q, sets:from_list([{UY, UX}]), Candidates, Map).

bfs(Reachables, Q, Visited, Candidates, Map) ->
    case queue:out(Q) of
        {empty, _} ->
            Reachables;
        {{value, {Y, X, N}}, Q2} ->
            F = fun({DY, DX}, {ReachablesAcc, QAcc, VisitedAcc} = Acc) ->
                        {YY, XX} = {Y + DY, X + DX},
                        YX = {YY, XX},
                        case sets:is_element(YX, Map)
                            andalso (not sets:is_element(YX, Visited)) of
                            false ->
                                Acc;
                            true ->
                                NewReachablesAcc =
                                    case sets:is_element(YX, Candidates) of
                                        true ->
                                            [{N + 1, YY, XX} | ReachablesAcc];
                                        false ->
                                            ReachablesAcc
                                    end,
                                NewQAcc = queue:in({YY, XX, N + 1}, QAcc),
                                NewVisitedAcc = sets:add_element(YX,
                                                                 VisitedAcc),
                                {NewReachablesAcc, NewQAcc, NewVisitedAcc}
                        end
                end,
            Init = {Reachables, Q2, Visited},
            {NewReachables, NewQ, NewVisited} = lists:foldl(F, Init, ?D),
            bfs(NewReachables, NewQ, NewVisited, Candidates, Map)
    end.

move(Map, {{UY, UX}, Attrs}, Y, X) ->
    {NY, NX} = next(UY, UX, Y, X, Map),
    {sets:add_element({UY, UX}, sets:del_element({NY, NX}, Map)),
     {{NY, NX}, Attrs}}.

next(UY, UX, Y, X, Map) ->
    Q = queue:from_list([{Y, X, 0}]),
    Weights = bfs(Q, maps:from_list([{{Y, X}, 0}]), Map),
    F = fun({DY, DX}, Min) ->
                {YY, XX} = {UY + DY, UX + DX},
                W = maps:get({YY, XX}, Weights, undefined),
                min(Min, {W, YY, XX})
        end,
    {_, RY, RX} = lists:foldl(F, [], ?D),
    {RY, RX}.

bfs(Q, Visited, Map) ->
    case queue:out(Q) of
        {empty, _} ->
            Visited;
        {{value, {Y, X, N}}, Q2} ->
            F = fun({DY, DX}, {QAcc, VisitedAcc} = Acc) ->
                        {YY, XX} = {Y + DY, X + DX},
                        YX = {YY, XX},
                        case sets:is_element(YX, Map)
                            andalso (not maps:is_key(YX, Visited)) of
                            false ->
                                Acc;
                            true ->
                                NewQAcc = queue:in({YY, XX, N + 1}, QAcc),
                                NewVisitedAcc = maps:put(YX, N + 1, VisitedAcc),
                                {NewQAcc, NewVisitedAcc}
                        end
                end,
            {NewQ, NewVisited} = lists:foldl(F, {Q2, Visited}, ?D),
            bfs(NewQ, NewVisited, Map)
    end.

attack({{UY, UX}, {_, Faction}}, Map, Acc, T, Targets, Attack) ->
    F = fun({DY, DX}, Min) ->
                case lists:keyfind({UY + DY, UX + DX}, 1, Targets) of
                    false ->
                        Min;
                    {{Y, X}, {HP, Fac}} ->
                        min(Min, {HP, Y, X, Fac})
                end
        end,
    ATK = atk(Faction, Attack),
    case lists:foldl(F, [], ?D) of
        [] ->
            {Map, Acc, T};
        {THP, TY, TX, _} when THP =< ATK ->
            YX = {TY, TX},
            {sets:add_element(YX, Map),
             lists:keydelete(YX, 1, Acc),
             lists:keydelete(YX, 1, T)};
        {THP, TY, TX, Fac} ->
            YX = {TY, TX},
            NewTarget = {YX, {THP - ATK, Fac}},
            {Map,
             lists:keyreplace(YX, 1, Acc, NewTarget),
             lists:keyreplace(YX, 1, T, NewTarget)}
    end.


-define(ATTACK, 3).

atk(goblin, _) ->
    ?ATTACK;
atk(elf, Attack) ->
    Attack.
