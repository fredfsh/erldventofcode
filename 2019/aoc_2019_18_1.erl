-module(aoc_2019_18_1).

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
    Memo = dists(State),
    bfs(0, maps:from_list([{0, [State]}]), Memo).

dists(#state{xy = XY0, pos = Pos}) ->
    F = fun(XY, C) when C >= $a , C =< $z ->
                {true, dist(XY, Pos)};
           (_, _) ->
                false
        end,
    maps:put(XY0, dist(XY0, Pos), maps:filtermap(F, Pos)).

dist(XY0, Pos) ->
    Q = queue:from_list([{XY0, 0, sets:new()}]),
    Visited = sets:from_list([XY0]),
    dist_impl([], Q, Visited, Pos).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

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

sig(#state{xy = XY, kds = KDs}) ->
    {lists:sort(maps:keys(KDs)), XY}.

search(#state{xy = XY0, kds = KDs} = State, Q, Memo, Seen) ->
    F = fun({Key, Dist, _, Doors}, Acc) ->
                G = fun(Door) -> maps:is_key(Door, KDs) end,
                case sets:is_element(sig(State), Seen)
                    orelse (not maps:is_key(Key, KDs))
                    orelse (not sets:is_empty(sets:filter(G, Doors))) of
                    true ->
                        Acc;
                    false ->
                        NewState = pickup(Key, Dist, State),
                        case sets:is_element(sig(NewState), Seen) of
                            true ->
                                Acc;
                            false ->
                                #state{n = N} = NewState,
                                H = fun(L) -> [NewState | L] end,
                                maps:update_with(N, H, [NewState], Acc)
                        end
                end
        end,
    lists:foldl(F, Q, maps:get(XY0, Memo)).

pickup(Key, Dist, #state{kds = KDs, pos = Pos, n = N}) ->
    {XY, KDs2} = maps:take(Key, KDs),
    Door = Key - $a + $A,
    {NewKDs, NewPos} = case maps:take(Door, KDs2) of
                           error ->
                               {KDs2, maps:remove(XY, Pos)};
                           {DoorXY, NKDs} ->
                               {NKDs, maps:remove(XY, maps:remove(DoorXY, Pos))}
                       end,
    #state{xy = XY, kds = NewKDs, pos = NewPos, n = N + Dist}.
