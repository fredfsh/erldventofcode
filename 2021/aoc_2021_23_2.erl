-module(aoc_2021_23_2).

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
    case io:get_line("") of
        eof ->
            eof;
        _ ->
            io:get_line(""),
            State0 = array:from_list(["DD", "CB", "BA", "AC"]),
            {ok, L1} = io:fread("", "###~c#~c#~c#~c###"),
            Seq = lists:seq(0, 3),
            F = fun({[X], I}, Acc) ->
                        L = array:get(I, Acc),
                        array:set(I, [X | L], Acc)
                end,
            State1 = lists:foldl(F, State0, lists:zip(L1, Seq)),
            {ok, L2} = io:fread("", " #~c#~c#~c#~c#"),
            H = fun({[X], I}, Acc) ->
                        L = array:get(I, Acc),
                        array:set(I, L ++ [X], Acc)
                end,
            State2 = lists:foldl(H, State1, lists:zip(L2, Seq)),
            io:get_line(""),
            io:get_line(""),
            {State2, array:new(11, {default, undefined})}

    end.

ini() ->
    0.

do(State) ->
    bfs(0, maps:from_list([{0, sets:from_list([State])}]), sets:new()).

-define(TARGET, [[$A, $A, $A, $A],
                 [$B, $B, $B, $B],
                 [$C, $C, $C, $C],
                 [$D, $D, $D, $D]]).

bfs(I, Q, Seen) ->
    case maps:take(I, Q) of
        error ->
            bfs(I + 1, Q, Seen);
        {States, Q2} ->
            F = fun({Doors, _} = State, {QAcc, SeenAcc} = Acc) ->
                        case array:to_list(Doors) of
                            ?TARGET ->
                                I;
                            _ ->
                                case sets:is_element(State, SeenAcc) of
                                    true ->
                                        Acc;
                                    false ->
                                        {bfs_impl(I, State, QAcc, SeenAcc),
                                         sets:add_element(State, SeenAcc)}
                                end
                        end;
                   (_, Res) ->
                        Res
                end,
            case sets:fold(F, {Q2, Seen}, States) of
                {NewQ, NewSeen} ->
                    bfs(I + 1, NewQ, NewSeen);
                Res ->
                    Res
            end
    end.

bfs_impl(I, State, Q, Seen) ->
    NQ = bfs_doors(I, State, Q, Seen),
    bfs_hallway(I, State, NQ, Seen).

-define(H, [0, 1, 3, 5, 7, 9, 10]).

bfs_doors(Cost, {Doors, Hallway}, Q, Seen) ->
    F = fun(_, [], Acc) ->
                Acc;
           (I, [H], Acc) when H - $A =:= I ->
                Acc;
           (I, [H, H], Acc) when H - $A =:= I ->
                Acc;
           (I, [H, H, H], Acc) when H - $A =:= I ->
                Acc;
           (I, [H, H, H, H], Acc) when H - $A =:= I ->
                Acc;
           (I, [H | T], Acc) ->
                G = fun(HPos, QAcc) ->
                            case move_to_hallway(HPos, I, Hallway) of
                                false ->
                                    QAcc;
                                true ->
                                    NewState = {array:set(I, T, Doors),
                                                array:set(HPos, H, Hallway)},
                                    case sets:is_element(NewState, Seen) of
                                        true ->
                                            QAcc;
                                        false ->
                                            NewCost = cost(Cost, I, T, HPos, H),
                                            enqueue(NewCost, NewState, QAcc)
                                    end
                            end
                    end,
                lists:foldl(G, Acc, ?H)
        end,
    array:foldl(F, Q, Doors).

move_to_hallway(HPos, D, Hallway) ->
    DPos = dpos(D),
    Min = min(HPos, DPos),
    Max = max(HPos, DPos),
    F = fun(I) -> array:get(I, Hallway) =:= undefined end,
    lists:all(F, lists:seq(Min, Max)).

cost(Base, D, L, HPos, H) ->
    N = abs(HPos - dpos(D)) + (4 - length(L)),
    Base + cost(H) * N.

cost($A) -> 1;
cost($B) -> 10;
cost($C) -> 100;
cost($D) -> 1000.

dpos(D) -> D * 2 + 2.

enqueue(Cost, State, Q) ->
    F = fun(Set) -> sets:add_element(State, Set) end,
    maps:update_with(Cost, F, sets:from_list([State]), Q).

bfs_hallway(Cost, {Doors, Hallway}, Q, Seen) ->
    F = fun(_, undefined, Acc) ->
                Acc;
           (I, X, Acc) ->
                D = X - $A,
                case array:get(D, Doors) of
                    L when L =:= []; L =:= [X]; L =:= [X, X]; L =:= [X, X, X] ->
                        case move_to_doors(D, I, Hallway) of
                            false ->
                                Acc;
                            true ->
                                NewState = {array:set(D, [X | L], Doors),
                                            array:set(I, undefined, Hallway)},
                                case sets:is_element(NewState, Seen) of
                                    true ->
                                        Acc;
                                    false ->
                                        NewCost = cost(Cost, D, L, I, X),
                                        enqueue(NewCost, NewState, Q)
                                end
                        end;
                    _ ->
                        Acc
                end
        end,
    array:foldl(F, Q, Hallway).

move_to_doors(D, I, Hallway) ->
    {DPos, HPos} = case dpos(D) of
                       J when J < I ->
                           {J, I - 1};
                       J when J > I ->
                           {J, I + 1}
                   end,
    Min = min(HPos, DPos),
    Max = max(HPos, DPos),
    F = fun(H) -> array:get(H, Hallway) =:= undefined end,
    lists:all(F, lists:seq(Min, Max)).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
