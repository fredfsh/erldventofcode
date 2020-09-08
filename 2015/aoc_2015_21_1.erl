-module(aoc_2015_21_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    {ok, [HP]} = io:fread("", "Hit Points: ~d"),
    {ok, [Atk]} = io:fread("", "Damage: ~d"),
    {ok, [Def]} = io:fread("", "Armor: ~d"),
    {HP, Atk, Def}.

-define(WEAPONS, [{ 8, 4, 0},
                  {10, 5, 0},
                  {25, 6, 0},
                  {40, 7, 0},
                  {74, 8, 0}]).
-define(ARMORS, [{ 13, 0, 1},
                 { 31, 0, 2},
                 { 53, 0, 3},
                 { 75, 0, 4},
                 {102, 0, 5}]).
-define(RINGS,  [{ 25, 1, 0},
                 { 50, 2, 0},
                 {100, 3, 0},
                 { 20, 0, 1},
                 { 40, 0, 2},
                 { 80, 0, 3}]).

do(In) ->
    pick_weapon(10000, 0, {100, 0, 0}, In).

pick_weapon(Min, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}) ->
    FW = fun({WCost, WAtk, WDef}, Acc) ->
                 pick_armor(Acc, Cost + WCost,
                            {PHP, PAtk + WAtk, PDef + WDef},
                            {OHP, OAtk, ODef})
         end,
    lists:foldl(FW, Min, ?WEAPONS).

pick_armor(Min, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}) ->
    MinA = pick_rings(Min, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}),
    FA = fun({ACost, AAtk, ADef}, Acc) ->
                 pick_rings(Acc, Cost + ACost,
                            {PHP, PAtk + AAtk, PDef + ADef},
                            {OHP, OAtk, ODef})
         end,
    lists:foldl(FA, MinA, ?ARMORS).

pick_rings(Min, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}) ->
    MinR =
        case win(PHP, PAtk, PDef, OHP, OAtk, ODef) of
            true ->
                min(Min, Cost);
            _ ->
                Min
        end,
    FR = fun({RCost, RAtk, RDef}, Acc) ->
                 NewCost = Cost + RCost,
                 NewPAtk = PAtk + RAtk,
                 NewPDef = PDef + RDef,
                 case win(PHP, NewPAtk, NewPDef, OHP, OAtk, ODef) of
                     true ->
                         NewAcc = min(Acc, NewCost),
                         FR2 = fun({R2Cost, R2Atk, R2Def}, MinAcc) ->
                                       case win(PHP,
                                                NewPAtk + R2Atk,
                                                NewPDef + R2Def,
                                                OHP,
                                                OAtk,
                                                ODef) of
                                           true ->
                                               min(MinAcc, NewCost + R2Cost);
                                           _ ->
                                               MinAcc
                                       end
                               end,
                         lists:foldl(FR2, NewAcc, ?RINGS);
                     _ ->
                         Acc
                 end
         end,
    lists:foldl(FR, MinR, ?RINGS).

win(PHP, PAtk, PDef, OHP, OAtk, ODef) ->
    ODmg = max(1, OAtk - PDef),
    OTurns = (PHP + (ODmg - 1)) div ODmg,
    PDmg = max(1, PAtk - ODef),
    PDmg * OTurns >= OHP.
