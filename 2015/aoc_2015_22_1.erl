-module(aoc_2015_22_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    {ok, [HP]} = io:fread("", "Hit Points: ~d"),
    {ok, [Atk]} = io:fread("", "Damage: ~d"),
    {HP, Atk}.

do({OHP, Atk}) ->
    bfs(OHP, Atk).

-record(state, {spend = 0, php = 50, mana = 500, ohp, atk,
                shield = 0, poison = 0, recharge = 0}).

bfs(OHP, Atk) ->
    bfs_impl(1000000, [#state{ohp = OHP, atk = Atk}], []).

-define(SPELLS, [fun magic_missle/1,
                 fun drain/1,
                 fun shield/1,
                 fun poison/1,
                 fun recharge/1]).

bfs_impl(Min, [], []) ->
    Min;
bfs_impl(Min, [], L) ->
    bfs_impl(Min, L, []);
bfs_impl(Min, [State | T], L) ->
    F = fun(Spell, {MinAcc, LAcc}) ->
                case Spell(State) of
                    lose ->
                        {MinAcc, LAcc};
                    {win, Spend} ->
                        {min(MinAcc, Spend), LAcc};
                    #state{spend = Spend} = NewState when Spend < MinAcc ->
                        {MinAcc, [NewState | LAcc]};
                    _ ->
                        {MinAcc, LAcc}
                end
        end,
    {NewMin, NewL} = lists:foldl(F, {Min, L}, ?SPELLS),
    bfs_impl(NewMin, T, NewL).

magic_missle(#state{mana = Mana}) when Mana < 53 ->
    lose;
magic_missle(State) ->
    case apply_effects(State) of
        #state{ohp = HP, spend = S, mana = Mana} = State2 ->
            boss_turn(State2#state{ohp = HP - 4,
                                   spend = S + 53, mana = Mana - 53});
        Ending ->
            Ending
    end.

drain(#state{mana = Mana}) when Mana < 73 ->
    lose;
drain(State) ->
    case apply_effects(State) of
        #state{php = HP1, ohp = HP2, spend = S, mana = Mana} = State2 ->
            boss_turn(State2#state{php = HP1 + 2, ohp = HP2 - 2,
                                   spend = S + 73, mana = Mana - 73});
        Ending ->
            Ending
    end.

shield(#state{mana = Mana}) when Mana < 113 ->
    lose;
shield(#state{shield = S}) when S > 1 ->
    lose;
shield(State) ->
    case apply_effects(State) of
        #state{spend = S, mana = Mana} = State2 ->
            boss_turn(State2#state{shield = 6,
                                   spend = S + 113, mana = Mana - 113});
        Ending ->
            Ending
    end.

poison(#state{mana = Mana}) when Mana < 173 ->
    lose;
poison(#state{poison = P}) when P > 1 ->
    lose;
poison(State) ->
    case apply_effects(State) of
        #state{spend = S, mana = Mana} = State2 ->
            boss_turn(State2#state{poison = 6,
                                   spend = S + 173, mana = Mana - 173});
        Ending ->
            Ending
    end.

recharge(#state{mana = Mana}) when Mana < 229 ->
    lose;
recharge(#state{recharge = R}) when R > 1 ->
    lose;
recharge(State) ->
    case apply_effects(State) of
        #state{spend = S, mana = Mana} = State2 ->
            boss_turn(State2#state{recharge = 5,
                                   spend = S + 229, mana = Mana - 229});
        Ending ->
            Ending
    end.

apply_effects(#state{shield = Shield, poison = Poison, recharge = Recharge,
                     mana = Mana, ohp = HP, spend = Spend}
              = State) ->
    State2 = State#state{shield = max(0, Shield - 1)},
    State3 =
        case Recharge of
            0 ->
                State2;
            R ->
                State2#state{mana = Mana + 101, recharge = R - 1}
        end,
    case Poison of
        0 ->
            State3;
        _ when HP =< 3 ->
            {win, Spend};
        P ->
            State3#state{ohp = HP - 3, poison = P - 1}
    end.

boss_turn(#state{ohp = HP, spend = Spend}) when HP =< 0 ->
    {win, Spend};
boss_turn(State) ->
    case apply_effects(State) of
        #state{php = HP, shield = S, atk = Atk} = State2 when S > 0 ->
            case HP =< max(1, Atk - 7) of
                true ->
                    lose;
                _ ->
                    State2#state{php = HP - max(1, Atk - 7)}
            end;
        #state{php = HP, atk = Atk} when HP =< Atk ->
            lose;
        #state{php = HP, atk = Atk} = State2 ->
            State2#state{php = HP - Atk};
        Ending ->
            Ending
    end.
