-module(aoc_2024_8_2).

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
    G = fun({X1, Y1, X2, Y2}, GAcc) ->
                add_antinodes(GAcc, X1, Y1, X2, Y2, Rows, Cols)
        end,
    F = fun(_, XYs, FAcc) ->
                L0 = sets:to_list(XYs),
                L1 = [{X1, Y1, X2, Y2}
                      || {X1, Y1} <- L0, {X2, Y2} <- L0, {X1, Y1} =/= {X2, Y2}],
                lists:foldl(G, FAcc, L1)
        end,
    maps:fold(F, sets:new(), Antennas).

add_antinodes(Acc, X1, Y1, X2, Y2, Rows, Cols) ->
    GCD = gcd(X2 - X1, Y2 - Y1),
    DX = (X2 - X1) div GCD,
    DY = (Y2 - Y1) div GCD,
%%    io:format("Adding antinodes {~p+~p, ~p+~p}~n", [X1, Y1, DX, DY]),
    add_antinodes_impl(Acc, X1, Y1, DX, DY, Rows, Cols).

gcd(0, Y) ->
    abs(Y);
gcd(X, 0) ->
    abs(X);
gcd(X, Y) when X < 0 ->
    gcd(-X, Y);
gcd(X, Y) when Y < 0 ->
    gcd(X, -Y);
gcd(X, Y) when X < Y ->
    gcd(Y, X);
gcd(X, Y) when X rem Y =:= 0 ->
    Y;
gcd(X, Y) ->
    gcd(Y, X rem Y).

add_antinodes_impl(Acc, X, Y, _, _, Rows, Cols)
  when X < 0 orelse X >= Cols orelse Y < 0 orelse Y >= Rows ->
    Acc;
add_antinodes_impl(Acc, X, Y, DX, DY, Rows, Cols) ->
    NAcc = sets:add_element({X, Y}, Acc),
    add_antinodes_impl(NAcc, X + DX, Y + DY, DX, DY, Rows, Cols).
