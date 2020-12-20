-module(aoc_2020_20_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Tiles = input_tiles(),
    Map = edges(Tiles),
    T4 = corner_tiles(Tiles, Map),
    multiply(T4).

input_tiles() ->
    input_tiles_impl([]).

input_tiles_impl(L) ->
    case io:fread("", "Tile ~d:\n") of
        eof ->
            L;
        {ok, [ID]} ->
            L4 = input_edges(),
            input_tiles_impl([{ID, L4} | L])
    end.

input_edges() ->
    Ls = input_edges_impl([]),
    [L1 | _] = Ls,
    L2 = lists:last(Ls),
    L3 = [First || [First | _] <- Ls],
    L4 = [lists:last(L) || L <- Ls],
    {L1, L2, L3, L4}.

input_edges_impl(Acc) ->
    case io:get_line("") of
        "\n" ->
            Acc;
        eof ->
            Acc;
        L ->
            L2 = string:trim(L, trailing),
            input_edges_impl([L2 | Acc])
    end.

edges(Tiles) ->
    F = fun({ID, {L1, L2, L3, L4}}, MapAccF) ->
                Edges = [L1, lists:reverse(L1),
                         L2, lists:reverse(L2),
                         L3, lists:reverse(L3),
                         L4, lists:reverse(L4)],
                G = fun(L, MapAccG) ->
                            H = fun(List) -> [ID | List] end,
                            maps:update_with(L, H, [ID], MapAccG)
                    end,
                lists:foldl(G, MapAccF, Edges)
        end,
    lists:foldl(F, maps:new(), Tiles).

corner_tiles(Tiles, Map) ->
    G = fun(L, Acc) ->
                case maps:get(L, Map) of
                    [_] ->
                        Acc + 1;
                    _ ->
                        Acc
                end
        end,
    F = fun({ID, {L1, L2, L3, L4}}, Acc) ->
                case lists:foldl(G, 0, [L1, L2, L3, L4]) of
                    2 ->
                        [ID | Acc];
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, [], Tiles).

multiply([A, B, C, D]) ->
    A * B * C * D.
