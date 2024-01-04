-module(aoc_2023_22_2).

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
        {ok, [S]} ->
            parse_brick(S)
    end.

-record(brick, {id, x, y, z, d, l}).

parse_brick(S) ->
    [S1, S2] = string:split(S, "~"),
    [X1, Y1, Z1] = parse_xyz(S1),
    [X2, Y2, Z2] = parse_xyz(S2),
    X = min(X1, X2),
    Y = min(Y1, Y2),
    Z = min(Z1, Z2),
    {Dir, Len} = case {X1, X2, Y1, Y2, Z1, Z2} of
                     {_, _, YY, YY, ZZ, ZZ} ->
                         {x, max(X1, X2) - X};
                     {XX, XX, _, _, ZZ, ZZ} ->
                         {y, max(Y1, Y2) - Y};
                     {XX, XX, YY, YY, _, _} ->
                         {z, max(Z1, Z2) - Z}
                 end,
    #brick{x = X, y = Y, z = Z, d = Dir, l = Len}.

parse_xyz(S) ->
    [list_to_integer(P) || P <- string:split(S, ",", all)].

ini() ->
    {0, []}.

do(X) ->
    X.

acc({ID, Bricks}, Brick) ->
    {ID + 1, [Brick#brick{id = ID} | Bricks]}.

fin({_, Snapshot}) ->
    {Bricks, Indexes} = settle(Snapshot),
    fin_impl(Bricks, Indexes).

settle(Snapshot) ->
    Sorted = lists:keysort(#brick.z, Snapshot),
    F = fun(#brick{id = ID} = Brick, {BricksAcc, IndexesAcc}) ->
                Rested = rest(Brick, IndexesAcc),
                {maps:put(ID, Rested, BricksAcc), index(IndexesAcc, Rested)}
        end,
    lists:foldl(F, {maps:new(), maps:new()}, Sorted).

rest(#brick{z = 1} = Brick, _) ->
    Brick;
rest(#brick{z = Z} = Brick, Indexes) ->
    F = fun(XYZ) -> maps:is_key(XYZ, Indexes) end,
    case lists:any(F, shadow(Brick)) of
        true ->
            Brick;
        false ->
            rest(Brick#brick{z = Z - 1}, Indexes)
    end.

shadow(#brick{x = X, y = Y, z = Z, d = z}) ->
    [{X, Y, Z - 1}];
shadow(#brick{z = Z} = Brick) ->
    xyzs(Brick#brick{z = Z - 1}).

xyzs(#brick{l = Len} = Brick) ->
    [xyz(Brick, N) || N <- lists:seq(0, Len)].

xyz(#brick{x = X, y = Y, z = Z, d = x}, N) ->
    {X + N, Y, Z};
xyz(#brick{x = X, y = Y, z = Z, d = y}, N) ->
    {X, Y + N, Z};
xyz(#brick{x = X, y = Y, z = Z, d = z}, N) ->
    {X, Y, Z + N}.

index(Indexes, #brick{id = ID} = Brick) ->
    F = fun(XYZ, Acc) -> maps:put(XYZ, ID, Acc) end,
    lists:foldl(F, Indexes, xyzs(Brick)).

fin_impl(Bricks, Indexes) ->
    %%io:format("~p~n~p~n", [Bricks, Indexes]),
    {Supportees, Supportors} = support(Bricks, Indexes),
    lists:sum([disintegrate(Supportees, Supportors, ID)
               || ID <- maps:keys(Supportees)]).

support(Bricks, Indexes) ->
    H = fun(X) ->
                fun(Set) -> sets:add_element(X, Set) end
        end,
    F = fun(#brick{id = Supportee} = Brick, FAcc) ->
                L1 = shadow(Brick),
                L2 = [maps:get(XYZ, Indexes, undefined) || XYZ <- L1],
                L3 = lists:usort(L2),
                G = fun(undefined, GAcc) ->
                            GAcc;
                       (Supportor, {SupporteesAcc, SupportorsAcc}) ->
                            {maps:update_with(Supportor,
                                              H(Supportee),
                                              sets:from_list([Supportee]),
                                              SupporteesAcc),
                             maps:update_with(Supportee,
                                              H(Supportor),
                                              sets:from_list([Supportor]),
                                              SupportorsAcc)}
                    end,
                lists:foldl(G, FAcc, L3)
        end,
    lists:foldl(F, {maps:new(), maps:new()}, maps:values(Bricks)).

disintegrate(Supportees, Supportors, ID) ->
    bfs(0, enqueue(queue:new(), ID, Supportees), Supportees, Supportors).

enqueue(Q, Supportor, Supportees) ->
    F = fun(Supportee, Acc) ->
                queue:in({Supportee, Supportor}, Acc)
        end,
    sets:fold(F, Q, maps:get(Supportor, Supportees, sets:new())).

bfs(Acc, Q, Supportees, Supportors) ->
    %%io:format("bfsing: ~p, ~p~n", [queue:to_list(Q), Supportors]),
    case queue:out(Q) of
        {empty, _} ->
            Acc;
        {{value, {Supportee, Supportor}}, Q2} ->
            F = fun(Set) -> sets:del_element(Supportor, Set) end,
            NewSupportors = maps:update_with(Supportee, F, Supportors),
            {NewAcc, NewQ} =
                case sets:size(maps:get(Supportee, NewSupportors)) of
                    0 ->
                        {Acc + 1, enqueue(Q2, Supportee, Supportees)};
                    _ ->
                        {Acc, Q2}
                end,
            bfs(NewAcc, NewQ, Supportees, NewSupportors)
    end.
