-module(aoc_2020_24_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

-define(TAB, tab).

run() ->
    ets:new(?TAB, [named_table]),
    run_impl().

run_impl() ->
    case io:get_line("") of
        eof ->
            turn(100),
            F = fun({_, Black}, Acc) -> Acc + Black end,
            ets:foldl(F, 0, ?TAB);
        L ->
            do(string:trim(L, trailing)),
            run_impl()
    end.

do(L) ->
    do_impl(L, 0, 0).

do_impl([], X, Y) ->
    ets:update_counter(?TAB, {Y, X}, {2, 1, 1, 0}, {{Y, X}, 0});
do_impl([$e | T], X, Y) ->
    do_impl(T, X + 2, Y);
do_impl([$w | T], X, Y) ->
    do_impl(T, X - 2, Y);
do_impl([$s, $e | T], X, Y) ->
    do_impl(T, X + 1, Y - 1);
do_impl([$s, $w | T], X, Y) ->
    do_impl(T, X - 1, Y - 1);
do_impl([$n, $e | T], X, Y) ->
    do_impl(T, X + 1, Y + 1);
do_impl([$n, $w | T], X, Y) ->
    do_impl(T, X - 1, Y + 1).

turn(0) ->
    ok;
turn(N) ->
    Updates = tiles_to_update(),
    update_tiles(Updates),
    turn(N - 1).

-define(ADJ, [{ 2,  0},
              {-2,  0},
              { 1, -1},
              {-1, -1},
              { 1,  1},
              {-1,  1}]).

tiles_to_update() ->
    F = fun({{Y, X}, 1}, {Checked, Updates}) ->
                G = fun({DX, DY}, {CheckedAcc, UpdatesAcc}) ->
                            NX = X + DX,
                            NY = Y + DY,
                            case sets:is_element({NY, NX}, Checked) of
                                true ->
                                    {CheckedAcc, UpdatesAcc};
                                _ ->
                                    NewUpdates =
                                        case flip(NX, NY) of
                                            true ->
                                                sets:add_element({NY, NX}, UpdatesAcc);
                                            _ ->
                                                UpdatesAcc
                                        end,
                                    {sets:add_element({NY, NX}, CheckedAcc), NewUpdates}
                            end
                    end,
                lists:foldl(G, {Checked, Updates}, [{0, 0} | ?ADJ]);
           (_, Acc) ->
                Acc
        end,
    {_, Res} = ets:foldl(F, {sets:new(), sets:new()}, ?TAB),
    Res.

flip(X, Y) ->
    Black = case ets:lookup(?TAB, {Y, X}) of
                [{_, 1}] ->
                    true;
                _ ->
                    false
            end,
    case black_adj(X, Y) of
        0 when Black ->
            true;
        N when N > 2, Black ->
            true;
        2 when not Black ->
            true;
        _ ->
            false
    end.

black_adj(X, Y) ->
    F = fun({DX, DY}, Acc) ->
                NX = X + DX,
                NY = Y + DY,
                case ets:lookup(?TAB, {NY, NX}) of
                    [{_, 1}] ->
                        Acc + 1;
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, 0, ?ADJ).

update_tiles(Tiles) ->
    F = fun({Y, X}, _) ->
                ets:update_counter(?TAB, {Y, X}, {2, 1, 1, 0}, {{Y, X}, 0})
        end,
    sets:fold(F, undefined, Tiles).
