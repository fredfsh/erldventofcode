-module(aoc_2018_22_2).

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
    case io:fread("", "depth: ~d target: ~d,~d") of
        eof ->
            eof;
        {ok, [Depth, X, Y]} ->
            {Depth, X, Y}
    end.

ini() ->
    0.

-define(EROSION, erosion).

do({Depth, TX, TY}) ->
    ets:new(?EROSION, [named_table]),
    Q = maps:from_list([{0, sets:from_list([{0, 0, torch}])}]),
    bfs(0, Q, sets:new(), Depth, TX, TY).

bfs(I, Q, Seen, Depth, TX, TY) ->
    case maps:take(I, Q) of
        error ->
            bfs(I + 1, Q, Seen, Depth, TX, TY);
        {States, Q2} ->
            case sets:is_element({TX, TY, torch}, States) of
                true ->
                    I;
                false ->
                    F = fun(State, {QAcc, SeenAcc} = Acc) ->
                                case sets:is_element(State, SeenAcc) of
                                    true ->
                                        Acc;
                                    false ->
                                        NewSeenAcc = sets:add_element(State,
                                                                      SeenAcc),
                                        NewQAcc = search(QAcc,
                                                         State,
                                                         I,
                                                         NewSeenAcc,
                                                         Depth,
                                                         TX,
                                                         TY),
                                        {NewQAcc, NewSeenAcc}
                                end
                        end,
                    {NewQ, NewSeen} = sets:fold(F, {Q2, Seen}, States),
                    bfs(I + 1, NewQ, NewSeen, Depth, TX, TY)
            end
    end.

-define(E, [torch, gear, neither]).
-define(D, [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]).

search(Q, {SX, SY, SE} = State, I, Seen, Depth, TX, TY) ->
    All = lists:append([{SX, SY, E} || E <- ?E],
                       [{SX + DX, SY + DY, SE} || {DX, DY} <- ?D]),
    F = fun(S, Acc) ->
                case sets:is_element(S, Seen) of
                    true ->
                        Acc;
                    false ->
                        case distance(State, S, Depth, TX, TY) of
                            undefined ->
                                Acc;
                            N ->
                                G = fun(Set) -> sets:add_element(S, Set) end,
                                Init = sets:from_list([S]),
                                maps:update_with(I + N, G, Init, Acc)
                        end
                end
        end,
    lists:foldl(F, Q, All).

distance(_, {X, _, _}, _, _, _) when X < 0 ->
    undefined;
distance(_, {_, Y, _}, _, _, _) when Y < 0 ->
    undefined;
distance(State, {X, Y, E}, Depth, TX, TY) ->
    Type = type(X, Y, Depth, TX, TY),
    case {State, legal(Type, E)} of
        {_, false} ->
            undefined;
        {{X, Y, _}, true} ->
            7;
        {{_, _, E}, true} ->
            1
    end.

type(X, Y, Depth, TX, TY) ->
    case erosion(X, Y, Depth, TX, TY) rem 3 of
        0 ->
            rocky;
        1 ->
            wet;
        2 ->
            narrow
    end.

erosion(X, Y, Depth, TX, TY) ->
    case ets:lookup(?EROSION, {X, Y}) of
        [] ->
            Erosion = (geology(X, Y, Depth, TX, TY) + Depth) rem 20183,
            ets:insert_new(?EROSION, {{X, Y}, Erosion}),
            Erosion;
        [{_, Erosion}] ->
            Erosion
    end.

geology(0, 0, _, _, _) ->
    0;
geology(TX, TY, _, TX, TY) ->
    0;
geology(X, 0, _, _, _) ->
    X * 16807;
geology(0, Y, _, _, _) ->
    Y * 48271;
geology(X, Y, Depth, TX, TY) ->
    erosion(X - 1, Y, Depth, TX, TY) * erosion(X, Y - 1, Depth, TX, TY).

legal(rocky, neither) -> false;
legal(wet, torch) -> false;
legal(narrow, gear) -> false;
legal(_, _) -> true.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
