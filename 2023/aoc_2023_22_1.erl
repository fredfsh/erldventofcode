-module(aoc_2023_22_1).

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
    F = fun(Brick, Acc) ->
                L1 = shadow(Brick),
                L2 = [maps:get(XYZ, Indexes, undefined) || XYZ <- L1],
                L3 = lists:usort(L2),
                case L3 of
                    [undefined] ->
                        Acc;
                    [ID] ->
                        %%io:format("~p unremovable supporting ~p~n", [ID, Brick#brick.id]),
                        sets:add_element(ID, Acc);
                    [ID, undefined] ->
                        %%io:format("~p unremovable supporting ~p~n", [ID, Brick#brick.id]),
                        sets:add_element(ID, Acc);
                    _ ->
                        Acc
                end
        end,
    Unremovable = lists:foldl(F, sets:new(), maps:values(Bricks)),
    %%io:format("~p~n", [Unremovable]),
    maps:size(Bricks) - sets:size(Unremovable).
