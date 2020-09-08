-module(aoc_2015_21_2).

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
    pick_weapon(0, 0, {100, 0, 0}, In).

pick_weapon(Max, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}) ->
    FW = fun({WCost, WAtk, WDef}, Acc) ->
                 pick_armor(Acc, Cost + WCost,
                            {PHP, PAtk + WAtk, PDef + WDef},
                            {OHP, OAtk, ODef})
         end,
    lists:foldl(FW, Max, ?WEAPONS).

pick_armor(Max, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}) ->
    MaxA = pick_rings(Max, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}),
    FA = fun({ACost, AAtk, ADef}, Acc) ->
                 pick_rings(Acc, Cost + ACost,
                            {PHP, PAtk + AAtk, PDef + ADef},
                            {OHP, OAtk, ODef})
         end,
    lists:foldl(FA, MaxA, ?ARMORS).

pick_rings(Max, Cost, {PHP, PAtk, PDef}, {OHP, OAtk, ODef}) ->
    MaxR =
        case loose(PHP, PAtk, PDef, OHP, OAtk, ODef) of
            true ->
                max(Max, Cost);
            _ ->
                Max
        end,
    FR = fun({RCost, RAtk, RDef}, Acc) ->
                 NewCost = Cost + RCost,
                 NewPAtk = PAtk + RAtk,
                 NewPDef = PDef + RDef,
                 case loose(PHP, NewPAtk, NewPDef, OHP, OAtk, ODef) of
                     true ->
                         NewAcc = max(Acc, NewCost),
                         FR2 = fun({R2Cost, R2Atk, R2Def}, MaxAcc) ->
                                       case loose(PHP,
                                                  NewPAtk + R2Atk,
                                                  NewPDef + R2Def,
                                                  OHP,
                                                  OAtk,
                                                  ODef) of
                                           true ->
                                               max(MaxAcc, NewCost + R2Cost);
                                           _ ->
                                               MaxAcc
                                       end
                               end,
                         lists:foldl(FR2, NewAcc, ?RINGS);
                     _ ->
                         Acc
                 end
         end,
    lists:foldl(FR, MaxR, ?RINGS).

loose(PHP, PAtk, PDef, OHP, OAtk, ODef) ->
    ODmg = max(1, OAtk - PDef),
    OTurns = (PHP + (ODmg - 1)) div ODmg,
    PDmg = max(1, PAtk - ODef),
    PDmg * OTurns < OHP.
