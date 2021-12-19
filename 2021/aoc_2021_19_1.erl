-module(aoc_2021_19_1).

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
            input_beacons()
    end.

input_beacons() ->
    input_beacons_impl(sets:new()).

input_beacons_impl(Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        "\n" ->
            Acc;
        L ->
            [Xs, Ys, Zs] = string:split(string:trim(L), ",", all),
            C = {list_to_integer(Xs), list_to_integer(Ys), list_to_integer(Zs)},
            input_beacons_impl(sets:add_element(C, Acc))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Arr) ->
    Rems = sets:from_list(lists:seq(0, array:size(Arr) - 1)),
    merge(Rems, Arr).

merge(Rems, Arr) ->
    case sets:size(Rems) of
        1 ->
            [ID] = sets:to_list(Rems),
            sets:size(array:get(ID, Arr));
        _ ->
            F = fun(RemID, undefined) ->
                        G = fun(ScannerID, undefined) when ScannerID =/= RemID ->
                                    case overlap(RemID, ScannerID, Arr) of
                                        false ->
                                            undefined;
                                        Merged ->
                                            {RemID, ScannerID, Merged}
                                    end;
                               (_, Acc) ->
                                    Acc
                            end,
                        sets:fold(G, undefined, Rems);
                   (_, Acc) ->
                        Acc
                end,
            {ID1, ID2, Beacons1} = sets:fold(F, undefined, Rems),
            merge(sets:del_element(ID2, Rems), array:set(ID1, Beacons1, Arr))
    end.

overlap(ID1, ID2, Arr) ->
    B1 = array:get(ID1, Arr),
    B2 = array:get(ID2, Arr),
    case sets:size(B1) > sets:size(B2) of
        true ->
            overlap_impl(0, B1, B2);
        false ->
            overlap_impl(0, B2, B1)
    end.

overlap_impl(24, _, _) ->
    false;
overlap_impl(Tr, Beacons0, Beacons) ->
    Rotated = tr(Tr, Beacons),
    case align(Beacons0, Rotated) of
        false ->
            overlap_impl(Tr + 1, Beacons0, Beacons);
        Aligned ->
            sets:union(Beacons0, Aligned)
    end.

tr(Tr, Beacons) ->
    F = fun(Beacon) ->
                Fun = tr(Tr),
                Fun(Beacon)
        end,
    sets_map(F, Beacons).

tr(0) -> fun({X, Y, Z}) -> {X, Y, Z} end;
tr(1) -> fun({X, Y, Z}) -> {X, Z, -Y} end;
tr(2) -> fun({X, Y, Z}) -> {X, -Y, -Z} end;
tr(3) -> fun({X, Y, Z}) -> {X, -Z, Y} end;

tr(4) -> fun({X, Y, Z}) -> {-X, -Z, -Y} end;
tr(5) -> fun({X, Y, Z}) -> {-X, Y, -Z} end;
tr(6) -> fun({X, Y, Z}) -> {-X, Z, Y} end;
tr(7) -> fun({X, Y, Z}) -> {-X, -Y, Z} end;

tr(8) -> fun({X, Y, Z}) -> {Z, X, Y} end;
tr(9) -> fun({X, Y, Z}) -> {-Y, X, Z} end;
tr(10) -> fun({X, Y, Z}) -> {-Z, X, -Y} end;
tr(11) -> fun({X, Y, Z}) -> {Y, X, -Z} end;

tr(12) -> fun({X, Y, Z}) -> {-Z, -Y, -X} end;
tr(13) -> fun({X, Y, Z}) -> {Y, -Z, -X} end;
tr(14) -> fun({X, Y, Z}) -> {Z, Y, -X} end;
tr(15) -> fun({X, Y, Z}) -> {-Y, Z, -X} end;

tr(16) -> fun({X, Y, Z}) -> {Y, Z, X} end;
tr(17) -> fun({X, Y, Z}) -> {Z, -Y, X} end;
tr(18) -> fun({X, Y, Z}) -> {-Y, -Z, X} end;
tr(19) -> fun({X, Y, Z}) -> {-Z, Y, X} end;

tr(20) -> fun({X, Y, Z}) -> {-Y, -X, -Z} end;
tr(21) -> fun({X, Y, Z}) -> {-Z, -X, Y} end;
tr(22) -> fun({X, Y, Z}) -> {Y, -X, Z} end;
tr(23) -> fun({X, Y, Z}) -> {Z, -X, -Y} end.

align(Beacons0, Beacons) ->
    F = fun({X0, Y0, Z0}, false) ->
                G = fun({X, Y, Z}) -> {X - X0, Y - Y0, Z - Z0} end,
                Rel0 = sets_map(G, Beacons0),
                try_align(sets:to_list(Beacons), Beacons, Rel0, {X0, Y0, Z0});
           (_, Acc) ->
                Acc
        end,
    sets:fold(F, false, Beacons0).

try_align([], _, _, _) ->
    false;
try_align([{Xb, Yb, Zb} | T], Beacons, Rel0, {X0, Y0, Z0}) ->
    F = fun({X, Y, Z}) -> {X - Xb, Y - Yb, Z - Zb} end,
    Rel = sets_map(F, Beacons),
    G = fun({X, Y, Z}) -> {X + X0, Y + Y0, Z + Z0} end,
    case sets:size(sets:intersection(Rel0, Rel)) of
        N when N >= 12 ->
            sets_map(G, Rel);
        _ ->
            try_align(T, Beacons, Rel0, {X0, Y0, Z0})
    end.

sets_map(Fun, Set) ->
    F = fun(X, Acc) ->
                sets:add_element(Fun(X), Acc)
        end,
    sets:fold(F, sets:new(), Set).
