-module(aoc_2022_24_2).

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
            X
    end.

ini() ->
    undefined.

do(X) ->
    X.

-record(blizzard, {x, y, dx = 0, dy = 0}).

acc(_, [_, $., $# | _]) ->
    {1, []};
acc({Y, Blizzards}, [$#, $#, $# | _] = L) ->
    {Y, Blizzards, length(L) - 1};
acc({Y, Blizzards}, Line) ->
    L = string:trim(Line, both, "#"),
    F = fun($., {X, Acc}) ->
                {X + 1, Acc};
           ($^, {X, Acc}) ->
                {X + 1, [#blizzard{x = X, y = Y, dy = -1} | Acc]};
           ($v, {X, Acc}) ->
                {X + 1, [#blizzard{x = X, y = Y, dy =  1} | Acc]};
           ($<, {X, Acc}) ->
                {X + 1, [#blizzard{x = X, y = Y, dx = -1} | Acc]};
           ($>, {X, Acc}) ->
                {X + 1, [#blizzard{x = X, y = Y, dx =  1} | Acc]}
        end,
    {_, NewBlizzards} = lists:foldl(F, {1, Blizzards}, L),
    {Y + 1, NewBlizzards}.

-define(STAB, states).

fin({YY, Blizzards, XX}) ->
    memo(Blizzards, XX, YY),
    Min1 = sim(0, 1, 1, XX - 1, YY - 1, XX, YY),
    ets:delete_all_objects(?STAB),
    Min2 = sim(Min1, XX - 1, YY - 1, 1, 1, XX, YY),
    ets:delete_all_objects(?STAB),
    Min3 = sim(Min2, 1, 1, XX - 1, YY - 1, XX, YY),
    Min3.

-define(HTAB, horizontal).
-define(VTAB, vertical).

memo(Blizzards, XX, YY) ->
    ets:new(?STAB, [named_table]),
    ets:new(?HTAB, [named_table]),
    ets:new(?VTAB, [named_table]),
    F = fun(#blizzard{x = X, y = Y, dx = DX, dy = 0}) ->
                G = fun(T) ->
                            FX = (X - 1 + T * DX + XX - 1) rem (XX - 1) + 1,
                            ets:insert(?HTAB, {{T, FX, Y}, undefined})
                    end,
                lists:foreach(G, lists:seq(0, XX - 1));
           (#blizzard{x = X, y = Y, dx = 0, dy = DY}) ->
                G = fun(T) ->
                            FY = (Y - 1 + T * DY + YY - 1) rem (YY - 1) + 1,
                            ets:insert(?VTAB, {{T, X, FY}, undefined})
                    end,
                lists:foreach(G, lists:seq(0, YY - 1))
        end,
    lists:foreach(F, Blizzards).

sim(Min, XS, YS, XG, YG, XX, YY) ->
    NewMin = wait(Min, XS, YS, XX, YY),
    case dfs(undefined, XS, YS, NewMin, XG, YG, XX, YY) of
        undefined ->
            sim(NewMin + 1, XS, YS, XG, YG, XX, YY);
        Res ->
            Res
    end.

wait(Min, XS, YS, XX, YY) ->
    case fatal(Min + 1, XS, YS, XX, YY) of
        false ->
            Min + 1;
        true ->
            wait(Min + 1, XS, YS, XX, YY)
    end.

fatal(Min, X, Y, XX, YY) ->
    TH = Min rem (XX - 1),
    TV = Min rem (YY - 1),
    case {ets:lookup(?HTAB, {TH, X, Y}), ets:lookup(?VTAB, {TV, X, Y})} of
        {[], []} ->
            false;
        _ ->
            true
    end.

-define(DF, [{0, 1}, {1, 0}, {0, -1}, {-1, 0}, {0, 0}]).
-define(DB, [{0, -1}, {-1, 0}, {0, 1}, {1, 0}, {0, 0}]).

dfs(Best, XG, YG, Min, XG, YG, _, _) ->
    min(Best, Min + 1);
dfs(Best, X, Y, Min, XG, YG, _, _) when Min+abs(XG-X)+abs(YG-Y)+1 >= Best ->
    Best;
dfs(Best, X, Y, Min, XG, YG, XX, YY) ->
    ets:insert(?STAB, {{Min, X, Y}, undefined}),
    D = case {XG, YG} of
            {1, 1} ->
                ?DB;
            _ ->
                ?DF
        end,
    F = fun({DX, DY}, Acc) ->
                {NX, NY} = {X + DX, Y + DY},
                case NX > 0 andalso NX < XX andalso NY > 0 andalso NY < YY
                    andalso (not fatal(Min + 1, NX, NY, XX, YY))
                    andalso (ets:lookup(?STAB, {Min + 1, NX, NY}) =:= []) of
                    true ->
                        min(Acc, dfs(Acc, NX, NY, Min + 1, XG, YG, XX, YY));
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, Best, D).
