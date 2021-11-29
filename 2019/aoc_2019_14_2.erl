-module(aoc_2019_14_2).

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
        L ->
            L2 = string:trim(L, trailing),
            [Left, Right] = string:split(L2, " => "),
            [{Target, TN}] = input_element(Right),
            Sources = input_element(Left),
            {Target, {TN, Sources}}
    end.

input_element(S) ->
    F = fun(X) ->
                {N, Res} = string:to_integer(X),
                {list_to_atom(string:trim(Res, leading)), N}
        end,
    lists:map(F, string:split(S, ", ", all)).

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, {X, Y}) ->
    maps:put(X, Y, Acc).

-define(TAB, memo).
-define(FUEL, 'FUEL').
-define(ORE, 'ORE').
-define(ORES, 1000000000000).

fin(Reactions) ->
    ets:new(?TAB, [named_table]),
    dp(?FUEL, Reactions),
    L = lists:reverse(lists:keysort(2, ets:tab2list(?TAB))),
    bsearch(0, ?ORES, Reactions, L).

dp(?ORE, _) ->
    0;
dp(Target, Reactions) ->
    case ets:lookup(?TAB, Target) of
        [{_, Depth}] ->
            Depth;
        [] ->
            {_, Sources} = maps:get(Target, Reactions),
            F = fun({Source, _}, Acc) ->
                        max(Acc, dp(Source, Reactions) + 1)
                end,
            Depth = lists:foldl(F, 0, Sources),
            ets:insert_new(?TAB, {Target, Depth}),
            Depth
    end.

bsearch(Left, Right, _, _) when Right =:= Left + 1 ->
    Left;
bsearch(Left, Right, Reactions, Depths) ->
    Mid = (Left + Right) div 2,
    case fuels(Mid, Reactions, Depths) =< ?ORES of
        true ->
            bsearch(Mid, Right, Reactions, Depths);
        false ->
            bsearch(Left, Mid, Reactions, Depths)
    end.

fuels(Fuels, Reactions, Depths) ->
    F = fun({Target, _}, Acc) ->
                Quantity = maps:get(Target, Acc),
                {Min, Sources} = maps:get(Target, Reactions),
                Times = (Quantity - 1 ) div Min + 1,
                G = fun({Source, N}, GAcc) ->
                            H = fun(V) -> V + N * Times end,
                            maps:update_with(Source, H, N * Times, GAcc)
                    end,
                lists:foldl(G, Acc, Sources)
        end,
    Map = lists:foldl(F, maps:from_list([{?FUEL, Fuels}]), Depths),
    maps:get(?ORE, Map).
