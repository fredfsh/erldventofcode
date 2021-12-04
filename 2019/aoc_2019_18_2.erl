-module(aoc_2019_18_2).

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
        {ok, [L]} ->
            L
    end.

-define(WALLS, walls).

-record(state, {xy, kds = maps:new(), pos = maps:new(), n = 0}).

ini() ->
    ets:new(?WALLS, [named_table]),
    {0, #state{}}.

do(X) ->
    X.

acc({Y, State}, L) ->
    F = fun($#, {StateAcc, X}) ->
                ets:insert_new(?WALLS, {{X, Y}, wall}),
                {StateAcc, X + 1};
           ($., {StateAcc, X}) ->
                {StateAcc, X + 1};
           ($@, {StateAcc, X}) ->
                {StateAcc#state{xy = {X, Y}}, X + 1};
           (C, {#state{kds = KDs, pos = Pos} = StateAcc, X}) ->
                XY = {X, Y},
                {StateAcc#state{kds = maps:put(C, XY, KDs),
                                pos = maps:put(XY, C, Pos)}, X + 1}
        end,
    {NewState, _} = lists:foldl(F, {State, 0}, L),
    {Y + 1, NewState}.

fin({_, State}) ->
    State2 = change_map(State),
    Memo = dists(State2),
    bfs(0, maps:from_list([{0, [State2]}]), Memo).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

change_map(#state{xy = {X, Y}} = State) ->
    Walls = [{X, Y} | [{X + DX, Y + DY} || {DX, DY} <- ?D]],
    ets:insert_new(?WALLS, [{W, wall} || W <- Walls]),
    State#state{xy = [{X+1, Y-1}, {X+1, Y+1}, {X-1, Y+1}, {X-1, Y-1}]}.

dists(#state{xy = XYs, pos = Pos}) ->
    F = fun(XY, C) when C >= $a , C =< $z ->
                {true, dist(XY, Pos)};
           (_, _) ->
                false
        end,
    G = fun(XY, Acc) -> maps:put(XY, dist(XY, Pos), Acc) end,
    lists:foldl(G, maps:filtermap(F, Pos), XYs).

dist(XY0, Pos) ->
    Q = queue:from_list([{XY0, 0, sets:new()}]),
    Visited = sets:from_list([XY0]),
    dist_impl([], Q, Visited, Pos).

dist_impl(Keys, Q, Visited, Pos) ->
    case queue:out(Q) of
        {empty, _} ->
            Keys;
        {{value, {{X, Y}, N, Doors}}, Q2} ->
            F = fun({DX, DY}, {QAcc, VisitedAcc, KeysAcc} = Acc) ->
                        NXY = {X + DX, Y + DY},
                        case visit(NXY, VisitedAcc, Pos) of
                            {key, Key} ->
                                NQAcc = queue:in({NXY, N + 1, Doors}, QAcc),
                                NVisitedAcc = sets:add_element(NXY, VisitedAcc),
                                NKeysAcc = [{Key, N + 1, NXY, Doors} | KeysAcc],
                                {NQAcc, NVisitedAcc, NKeysAcc};
                            {door, Door} ->
                                NDoors = sets:add_element(Door, Doors),
                                NQAcc = queue:in({NXY, N + 1, NDoors}, QAcc),
                                NVisitedAcc = sets:add_element(NXY, VisitedAcc),
                                {NQAcc, NVisitedAcc, KeysAcc};
                            false ->
                                Acc;
                            true ->
                                NQAcc = queue:in({NXY, N + 1, Doors}, QAcc),
                                NVisitedAcc = sets:add_element(NXY, VisitedAcc),
                                {NQAcc, NVisitedAcc, KeysAcc}
                        end
                end,
            {NQ, NVisited, NKeys} = lists:foldl(F, {Q2, Visited, Keys}, ?D),
            dist_impl(NKeys, NQ, NVisited, Pos)
    end.

visit(XY, Visited, Pos) ->
    case {sets:is_element(XY, Visited),
          maps:get(XY, Pos, undefined),
          ets:lookup(?WALLS, XY)} of
        {false, C, _} when C >= $a, C =< $z ->
            {key, C};
        {false, C, _} when C >= $A, C =< $Z ->
            {door, C};
        {false, undefined, []} ->
            true;
        _ ->
            false
    end.

bfs(N, Q, Memo) ->
    bfs_impl(N, Q, Memo, sets:new()).

bfs_impl(N, Q, Memo, Seen) ->
    case maps:take(N, Q) of
        error ->
            bfs_impl(N + 1, Q, Memo, Seen);
        {States, Q2} ->
            F = fun(_, Res) when is_integer(Res) ->
                        Res;
                   (#state{kds = KDs} = State, {QAcc, SeenAcc}) ->
                        case maps:size(KDs) of
                            0 ->
                                N;
                            _ ->
                                NQ = search(State, QAcc, Memo, SeenAcc),
                                NSeen = sets:add_element(sig(State), SeenAcc),
                                {NQ, NSeen}
                        end
                end,
            case lists:foldl(F, {Q2, Seen}, States) of
                Res when is_integer(Res) ->
                    Res;
                {NewQ, NewSeen} ->
                    bfs_impl(N + 1, NewQ, Memo, NewSeen)
            end
    end.

sig(#state{xy = XYs, kds = KDs}) ->
    {lists:sort(maps:keys(KDs)), XYs}.

search(State, Q, Memo, Seen) ->
    case sets:is_element(sig(State), Seen) of
        true ->
            Q;
        false ->
            search_impl(State, Q, Memo, Seen)
    end.

search_impl(#state{xy = XYs, kds = KDs} = State, Q, Memo, Seen) ->
    F = fun(XY, FAcc) ->
                G = fun({Key, Dist, _, Blockers}, GAcc) ->
                            H = fun(Door) -> maps:is_key(Door, KDs) end,
                            Doors = sets:filter(H, Blockers),
                            case maps:is_key(Key, KDs)
                                andalso sets:is_empty(Doors) of
                                true ->
                                    NewState = pickup(XY, Key, Dist, State),
                                    case sets:is_element(sig(NewState), Seen) of
                                        true ->
                                            GAcc;
                                        false ->
                                            #state{n = N} = NewState,
                                            I = fun(L) -> [NewState | L] end,
                                            maps:update_with(N,
                                                             I,
                                                             [NewState],
                                                             GAcc)
                                    end;
                                false ->
                                    GAcc
                            end
                    end,
                lists:foldl(G, FAcc, maps:get(XY, Memo))
        end,
    lists:foldl(F, Q, XYs).

pickup(XY0, Key, Dist, #state{xy = XYs, kds = KDs, pos = Pos, n = N}) ->
    {XY, KDs2} = maps:take(Key, KDs),
    NewXYs = replace(XYs, XY0, XY),
    Door = Key - $a + $A,
    {NewKDs, NewPos} = case maps:take(Door, KDs2) of
                           error ->
                               {KDs2, maps:remove(XY, Pos)};
                           {DoorXY, NKDs} ->
                               {NKDs, maps:remove(XY, maps:remove(DoorXY, Pos))}
                       end,
    #state{xy = NewXYs, kds = NewKDs, pos = NewPos, n = N + Dist}.

replace(L, From, To) ->
    replace_impl([], L, From, To).

replace_impl(Acc, [], _, _) ->
    lists:reverse(Acc);
replace_impl(Acc, [From | T], From, To) ->
    replace_impl([To | Acc], T, From, To);
replace_impl(Acc, [H | T], From, To) ->
    replace_impl([H | Acc], T, From, To).
