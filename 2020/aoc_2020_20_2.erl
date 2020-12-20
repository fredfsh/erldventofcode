-module(aoc_2020_20_2).

-export([start/0]).

-define(MONSTER, ["                  # ",
                  "#    ##    ##    ###",
                  " #  #  #  #  #  #   "]).
-define(MX, length(hd(?MONSTER))).
-define(MY, length(?MONSTER)).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    %% id -> tile
    Tiles = input_tiles(),
    %% edge -> set of ids
    Edges = edges(Tiles),

    Solution = solution(Tiles, Edges),
    Tile = tile(Solution),
    roughness(Tile).

input_tiles() ->
    input_tiles_impl(maps:new()).

input_tiles_impl(Map) ->
    case io:fread("", "Tile ~d:\n") of
        eof ->
            Map;
        {ok, [ID]} ->
            Tile = input_tile(),
            input_tiles_impl(maps:put(ID, Tile, Map))
    end.

input_tile() ->
    input_tile_impl([]).

input_tile_impl(Acc) ->
    case io:get_line("") of
        "\n" ->
            lists:reverse(Acc);
        eof ->
            lists:reverse(Acc);
        L ->
            L2 = string:trim(L, trailing),
            input_tile_impl([L2 | Acc])
    end.

%% returns map: edge -> list of tile ids
edges(Tiles) when is_map(Tiles) ->
    F = fun(ID, Tile, MapAccF) ->
                {N, S, W, E} = edges(Tile),
                G = fun(L, MapAccG) ->
                            H = fun(List) -> [ID | List] end,
                            maps:update_with(L, H, [ID], MapAccG)
                    end,
                lists:foldl(G, MapAccF, [N, lists:reverse(N),
                                         S, lists:reverse(S),
                                         W, lists:reverse(W),
                                         E, lists:reverse(E)])
        end,
    maps:fold(F, maps:new(), Tiles);
%% returns N, S, W, E edges of a given tile
edges(Tile) when is_list(Tile) ->
    N = hd(Tile),
    S = lists:last(Tile),
    W = [First || [First | _] <- Tile],
    E = [lists:last(L) || L <- Tile],
    {N, S, W, E}.
%% returns list of edges of a given tile that appear on boarder
edges(Tile, Edges) ->
    {N, S, W, E} = edges(Tile),
    F = fun(Edge, Acc) ->
                case maps:get(Edge, Edges) of
                    [_] ->
                        [Edge | Acc];
                    _ ->
                        Acc
                end
        end,
    lists:reverse(lists:foldl(F, [], [N, S, W, E])).

solution(Tiles, Edges) ->
    {ID, Tile} = top_left_corner(Tiles, Edges),
    N = round(math:sqrt(maps:size(Tiles))),
    solution_impl(2, 1, [Tile], [], sets:from_list([ID]), Tiles, Edges, N).

%% returns {ID, Tile} at top left corner
top_left_corner(Tiles, Edges) ->
    top_left_corner_impl(maps:iterator(Tiles), Edges).

top_left_corner_impl(It, Edges) ->
    {ID, Tile, NextIt} = maps:next(It),
    case edges(Tile, Edges) of
        [_, _] ->
            {ID, to_top_left_corner(Tile, Edges)};
        _ ->
            top_left_corner_impl(NextIt, Edges)
    end.

to_top_left_corner(Tile, Edges) ->
    to_top_left_corner_impl(transforms(), Tile, Edges).

transforms() -> [fun identity/1,
                 fun rotate_90/1,
                 fun rotate_180/1,
                 fun rotate_270/1,
                 fun flip/1,
                 fun flip_rotate_90/1,
                 fun flip_rotate_180/1,
                 fun flip_rotate_270/1].

identity(Tile) ->
    Tile.

rotate_90(Tile) ->
    rotate_90_impl(Tile, []).

rotate_90_impl([[] | _], Acc) ->
    lists:reverse(Acc);
rotate_90_impl(M, Acc) ->
    rotate_90_impl([tl(L) || L <- M], [lists:reverse([hd(L) || L <- M]) | Acc]).

rotate_180(Tile) ->
    rotate_90(rotate_90(Tile)).

rotate_270(Tile) ->
    rotate_90(rotate_180(Tile)).

flip(Tile) ->
    [lists:reverse(L) || L <- Tile].

flip_rotate_90(Tile) ->
    rotate_90(flip(Tile)).

flip_rotate_180(Tile) ->
    rotate_180(flip(Tile)).

flip_rotate_270(Tile) ->
    rotate_270(flip(Tile)).

to_top_left_corner_impl([H | T], Tile, Edges) ->
    Candidate = H(Tile),
    case {edges(Candidate), edges(Candidate, Edges)} of
        {{N, _S, W, _E}, [N, W]} ->
            Candidate;
        _ ->
            to_top_left_corner_impl(T, Tile, Edges)
    end.

solution_impl(1, _Y, _LAcc, Acc, _Used, _Tiles, _Edges, _N) when _Y =:= _N + 1 ->
    lists:reverse(Acc);
solution_impl(X, Y, LAcc, Acc, Used, Tiles, Edges, N) when X =:= N + 1 ->
    NewAcc = [lists:reverse(LAcc) | Acc],
    solution_impl(1, Y + 1, [], NewAcc, Used, Tiles, Edges, N);
solution_impl(1, Y, [], Acc, Used, Tiles, Edges, N) ->
    AboveTile = hd(hd(Acc)),
    {_, Common, _, _} = edges(AboveTile),
    {ID, Tile} = common_edge_tile(Common, 1, Used, Tiles, Edges),
    NewUsed = sets:add_element(ID, Used),
    solution_impl(2, Y, [Tile], Acc, NewUsed, Tiles, Edges, N);
solution_impl(X, Y, LAcc, Acc, Used, Tiles, Edges, N) ->
    LeftTile = hd(LAcc),
    {_, _, _, Common} = edges(LeftTile),
    {ID, Tile} = common_edge_tile(Common, 3, Used, Tiles, Edges),
    NewUsed = sets:add_element(ID, Used),
    solution_impl(X + 1, Y, [Tile | LAcc], Acc, NewUsed, Tiles, Edges, N).

common_edge_tile(Common, Index, Used, Tiles, Edges) ->
    {ID, Tile} = unused_tile(Common, Used, Tiles, Edges),
    {ID, common_edge_tile_impl(transforms(), Tile, Common, Index)}.

unused_tile(Edge, Used, Tiles, Edges) ->
    [ID1, ID2] = maps:get(Edge, Edges),
    ID = case sets:is_element(ID1, Used) of
             true ->
                 ID2;
             _ ->
                 ID1
         end,
    {ID, maps:get(ID, Tiles)}.

common_edge_tile_impl([H | T], Tile, Common, Index) ->
    Candidate = H(Tile),
    case element(Index, edges(Candidate)) of
        Common ->
            Candidate;
        _ ->
            common_edge_tile_impl(T, Tile, Common, Index)
    end.

tile(Solution) ->
    combine(remove_edges(Solution)).

remove_edges(Solution) ->
    F = fun(Tile) ->
                Tile1 = tl(Tile),
                Tile2 = lists:droplast(Tile1),
                Tile3 = [tl(L) || L <- Tile2],
                Tile4 = [lists:droplast(L) || L <- Tile3],
                Tile4
        end,
    [[F(Tile) || Tile <- TileL] || TileL <- Solution].

combine(Solution) ->
    lists:append([combine_impl(TileL, []) || TileL <- Solution]).

combine_impl([[] | _], Acc) ->
    lists:reverse(Acc);
combine_impl(Tiles, Acc) ->
    NewAcc = [lists:append([hd(Tile) || Tile <- Tiles]) | Acc],
    combine_impl([tl(Tile) || Tile <- Tiles], NewAcc).

roughness(Tile) ->
    Sharps = sharps(Tile),
    Monsters = monsters(Tile),
    Sharps - Monsters * sharps(?MONSTER).

sharps(L) when is_list(L) ->
    lists:sum([sharps(X) || X <- L]);
sharps($#) ->
    1;
sharps(_) ->
    0.

monsters(Tile) ->
    monsters_impl(transforms(), Tile).

monsters_impl([H | T], Tile) ->
    Candidate = H(Tile),
    case monsters_impl(Candidate) of
        N when N > 0 ->
            N;
        _ ->
            monsters_impl(T, Tile)
    end.

monsters_impl(Tile) ->
    N = length(Tile),
    monsters_impl(1, 1, 0, N, Tile).

monsters_impl(_X, Y, Acc, N, _Tile) when Y + ?MY - 1 > N ->
    Acc;
monsters_impl(X, Y, Acc, N, Tile) when X + ?MX - 1 > N ->
    monsters_impl(1, Y + 1, Acc, N, Tile);
monsters_impl(X, Y, Acc, N, Tile) ->
    NewAcc = case compare(1, 1, X, Y, Tile) of
                 true ->
                     Acc + 1;
                 _ ->
                     Acc
             end,
    monsters_impl(X + 1, Y, NewAcc, N, Tile).

compare(_X, Y, _SX, _SY, _Tile) when Y > ?MY ->
    true;
compare(X, Y, SX, SY, Tile) when X > ?MX ->
    compare(1, Y + 1, SX, SY, Tile);
compare(X, Y, SX, SY, Tile) ->
    case {lists:nth(X, lists:nth(Y, ?MONSTER)),
          lists:nth(SX + X - 1, lists:nth(SY + Y - 1, Tile))} of
        {$#, Ch} when Ch =/= $# ->
            false;
        {_, _} ->
            compare(X + 1, Y, SX, SY, Tile)
    end.
