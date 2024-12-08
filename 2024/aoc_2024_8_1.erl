-module(aoc_2024_8_1).

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
        {ok, [X]} ->
            array:from_list(X)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(X) ->
    antinodes(X).

antinodes(Graph) ->
    Antennas = antennas(Graph),
    Rows = array:size(Graph),
    Cols = array:size(array:get(0, Graph)),
    Antinodes = antinodes_impl(Antennas, Rows, Cols),
    sets:size(Antinodes).

antennas(Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(_, $., GAcc) ->
                            GAcc;
                       (X, Freq, GAcc) ->
                            H = fun(Val) -> sets:add_element({X, Y}, Val) end,
                            Init = sets:from_list([{X, Y}]),
                            maps:update_with(Freq, H, Init, GAcc)
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, maps:new(), Graph).

antinodes_impl(Antennas, Rows, Cols) ->
%%    io:format("~p~n", [Antennas]),
    G = fun({X, Y}) ->
                X >= 0 andalso X < Cols andalso Y >= 0 andalso Y < Rows
        end,
    F = fun(_, XYs, Acc) ->
                L0 = sets:to_list(XYs),
                L1 = [{X1 * 2 - X2, Y1 * 2 - Y2}
                      || {X1, Y1} <- L0, {X2, Y2} <- L0, {X1, Y1} =/= {X2, Y2}],
                L2 = lists:filter(G, L1),
                sets:union(Acc, sets:from_list(L2))
        end,
    maps:fold(F, sets:new(), Antennas).
