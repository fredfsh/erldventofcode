-module(aoc_2022_14_2).

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
        Line ->
            Parts = string:split(string:trim(Line), " -> ", all),
            F = fun(Part) ->
                        [XStr, YStr] = string:split(Part, ","),
                        {list_to_integer(XStr), list_to_integer(YStr)}
                end,
            lists:map(F, Parts)
    end.

ini() ->
    {sets:new(), 0}.

do(X) ->
    X.

acc({Rocks, MaxY}, [H | T]) ->
    F = fun({X, Y}, {RocksAcc, MYAcc, {X, LY}}) ->
                {add_rocks(RocksAcc, X, {min(LY, Y), max(LY, Y)}),
                 max(MYAcc, max(LY, Y)),
                 {X, Y}};
           ({X, Y}, {RocksAcc, MYAcc, {LX, Y}}) ->
                {add_rocks(RocksAcc, {min(LX, X), max(LX, X)}, Y),
                 max(MYAcc, Y),
                 {X, Y}}
        end,
    {ResRocks, ResMaxY, _} = lists:foldl(F, {Rocks, MaxY, H}, T),
    {ResRocks, ResMaxY}.

add_rocks(Rocks, X, {Y1, Y2}) ->
    F = fun(Y, Acc) ->
                sets:add_element({X, Y}, Acc)
        end,
    lists:foldl(F, Rocks, lists:seq(Y1, Y2));
add_rocks(Rocks, {X1, X2}, Y) ->
    F = fun(X, Acc) ->
                sets:add_element({X, Y}, Acc)
        end,
    lists:foldl(F, Rocks, lists:seq(X1, X2)).

fin(X) ->
    sim(X).

-define(SX, 500).
-define(SY, 0).

sim({Rocks, Y}) ->
    sim_sand(?SX, ?SY, sets:new(), Rocks, Y).

-define(solid(X, Y),
        (Y =:= MY + 2 orelse
         sets:is_element({X, Y}, Rocks) orelse
         sets:is_element({X, Y}, Sands))).

sim_sand(X, Y, Sands, Rocks, MY) ->
    case ?solid(X, Y + 1) of
        false ->
            sim_sand(X, Y + 1, Sands, Rocks, MY);
        true ->
            case ?solid(X - 1, Y + 1) of
                false ->
                    sim_sand(X - 1, Y + 1, Sands, Rocks, MY);
                true ->
                    case ?solid(X + 1, Y + 1) of
                        false ->
                            sim_sand(X + 1, Y + 1, Sands, Rocks, MY);
                        true when X =:= ?SX, Y =:= ?SY ->
                            sets:size(Sands) + 1;
                        true ->
                            sim_sand(?SX, ?SY, sets:add_element({X, Y}, Sands),
                                     Rocks, MY)
                    end
            end
    end.
