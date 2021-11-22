-module(aoc_2018_24_2).

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

-record(u, {id, faction, n, hp, atk, type, inv, immunes, weaks, target}).

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, ["Immune"]} ->
            io:get_line(""),
            immune;
        {ok, ["Infection:"]} ->
            infection;
        {ok, [UnitsStr]} ->
            N = list_to_integer(UnitsStr),
            {ok, [HP]} = io:fread("", " units each with ~d hit points"),
            {IMs, WKs} = case io:fread("", " ~c") of
                             {ok, ["w"]} ->
                                 {sets:new(), sets:new()};
                             {ok, ["("]} ->
                                 input_defensives()
                         end,
            P = "ith an attack that does ~d ~a damage at initiative ~d",
            {ok, [ATK, Type, INV]} = io:fread("", P),
            #u{n=N, hp=HP, atk=ATK, type=Type, inv=INV, immunes=IMs, weaks=WKs}
    end.

input_defensives() ->
    S = input_to_parenthesis(),
    Parts = string:split(S, ";"),
    F = fun(Part, {ImmunesAcc, WeaksAcc}) ->
                [H, _ | T] = string:split(string:trim(Part, leading), " ", all),
                Types = [list_to_atom(string:trim(X, trailing, ",")) || X <- T],
                case H of
                    "immune" ->
                        {sets:from_list(Types), WeaksAcc};
                    "weak" ->
                        {ImmunesAcc, sets:from_list(Types)}
                end
        end,
    Res = lists:foldl(F, {sets:new(), sets:new()}, Parts),
    io:fread("", " ~c"),
    Res.

input_to_parenthesis() ->
    input_to_parenthesis_impl([]).

input_to_parenthesis_impl(Acc) ->
    case io:fread("", "~c") of
        {ok, [")"]} ->
            lists:reverse(Acc);
        {ok, [[C]]} ->
            input_to_parenthesis_impl([C | Acc])
    end.

ini() ->
    {undefined, maps:new()}.

do(X) ->
    X.

acc({Faction, Units}, #u{} = Unit) ->
    ID = maps:size(Units),
    {Faction, maps:put(ID, Unit#u{id = ID, faction = Faction}, Units)};
acc({_, Units}, Faction) ->
    {Faction, Units}.

fin({_, Units}) ->
    Boost = bsearch(0, max_boost(Units), Units),
    {immune, Res} = sim(Units, Boost),
    Res.

max_boost(Units) ->
    F = fun(_, #u{faction = immune}, Acc) ->
                Acc;
           (_, #u{faction = infection, n = N, hp = HP}, Acc) ->
                max(Acc, 2 * N * HP)
        end,
    maps:fold(F, 0, Units).

bsearch(Left, Right, Units) ->
    case Left + 1 of
        Right ->
            Right;
        _ ->
            Mid = (Left + Right) div 2,
            case sim(Units, Mid) of
                loop ->
                    bsearch(Mid, Right, Units);
                {immune, _} ->
                    bsearch(Left, Mid, Units);
                {infection, _} ->
                    bsearch(Mid, Right, Units)
            end
    end.

sim(Units, Boost) ->
    sim_impl(Units, Boost, undefined).

sim_impl(Units, Boost, Last) ->
    case count(Units) of
        Last ->
            loop;
        {N, 0} ->
            {immune, N};
        {0, N} ->
            {infection, N};
        {_, _} = Count ->
            sim_impl(fight(Units, Boost), Boost, Count)
    end.

count(Units) ->
    F = fun(_, #u{faction = immune, n = N}, {ImmuneAcc, InfectionAcc}) ->
                {ImmuneAcc + N, InfectionAcc};
           (_, #u{faction = infection, n = N}, {ImmuneAcc, InfectionAcc}) ->
                {ImmuneAcc, InfectionAcc + N}
        end,
    maps:fold(F, {0, 0}, Units).

fight(Units, Boost) ->
    attack(target(Units, Boost), Boost).

target(Units, Boost) ->
    F = fun(#u{inv = INV1} = U1, #u{inv = INV2} = U2) ->
                {power(U1, Boost), INV1} >= {power(U2, Boost), INV2}
        end,
    Sorted = lists:sort(F, maps:values(Units)),
    Enemies = enemies(Sorted),
    H = fun(#u{id = I} = Unit, {UnitsAcc, TargetedAcc}) ->
                Target = target_impl(Unit, Enemies, TargetedAcc, Boost),
                {maps:put(I, Unit#u{target = Target}, UnitsAcc),
                 sets:add_element(Target, TargetedAcc)}
        end,
    {Res, _} = lists:foldl(H, {Units, sets:new()}, Sorted),
    Res.

power(#u{faction = immune, n = N, atk = ATK}, Boost) ->
    N * (ATK + Boost);
power(#u{faction = infection, n = N, atk = ATK}, _) ->
    N * ATK.

enemies(Units) ->
    lists:partition(fun(#u{faction = Faction}) -> Faction =:= immune end, Units).

target_impl(#u{faction = Faction, type = Type} = Attacker,
            {Immunes, Infections},
            Targeted,
            Boost) ->
    Power = power(Attacker, Boost),
    Enemies = case Faction of
                  immune ->
                      Infections;
                  infection ->
                      Immunes
              end,
    F = fun(#u{id = I, inv = INV, immunes = ITs, weaks = WTs} = Defender) ->
                DPower = power(Defender, Boost),
                case {sets:is_element(I, Targeted),
                      sets:is_element(Type, ITs),
                      sets:is_element(Type, WTs)} of
                    {true, _, _} ->
                        false;
                    {false, true, _} ->
                        false;
                    {false, false, true} ->
                        {true, {2 * Power, DPower, INV, I}};
                    {false, false, false} ->
                        {true, {Power, DPower, INV, I}}
                end
        end,
    case lists:filtermap(F, Enemies) of
        [] ->
            undefined;
        L ->
            {_, _, _, Res} = lists:last(lists:sort(L)),
            Res
    end.

attack(Units, Boost) ->
    F = fun(#u{inv = INV1}, #u{inv = INV2}) -> INV1 >= INV2 end,
    Sorted = lists:sort(F, maps:values(Units)),
    IDs = [ID || #u{id = ID} <- Sorted],
    G = fun(ID, Acc) ->
                case maps:get(ID, Acc, undefined) of
                    undefined ->
                        Acc;
                    #u{type = Type, target = Target} = Attacker ->
                        case maps:get(Target, Acc, undefined) of
                            undefined ->
                                Acc;
                            #u{id = I, n = DN, hp = HP, weaks = Weaks} = Unit ->
                                Power = power(Attacker, Boost),
                                Damage = case sets:is_element(Type, Weaks) of
                                             true ->
                                                 2 * Power;
                                             false ->
                                                 Power
                                         end,
                                case Damage div HP of
                                    Loss when Loss >= DN ->
                                        maps:remove(I, Acc);
                                    Loss ->
                                        maps:put(I, Unit#u{n = DN - Loss}, Acc)
                                end
                        end
                end
        end,
    lists:foldl(G, Units, IDs).
