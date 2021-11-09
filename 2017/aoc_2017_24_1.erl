-module(aoc_2017_24_1).

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
    case io:fread("", "~d/~d") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, {X, Y}) ->
    inc(Y, X, inc(X, Y, Acc)).

inc(K, V, Map) ->
    F = fun(Counts) -> maps:update_with(V, fun(N) -> N + 1 end, 1, Counts) end,
    maps:update_with(K, F, maps:from_list([{V, 1}]), Map).

fin(X) ->
    dfs(0, X).

dfs(Last, Map) ->
    case maps:get(Last, Map, undefined) of
        undefined ->
            0;
        Counts ->
            F = fun(Next, _, Max) ->
                        NewMap = dec(Last, Next, dec(Next, Last, Map)),
                        max(Max, Last + Next + dfs(Next, NewMap))
                end,
            maps:fold(F, 0, Counts)
    end.

dec(K, V, Map) ->
    Counts = maps:get(K, Map),
    NewCounts = case maps:get(V, Counts) of
                    1 ->
                        maps:remove(V, Counts);
                    N ->
                        maps:put(V, N - 1, Counts)
                end,
    case maps:size(NewCounts) of
        0 ->
            maps:remove(K, Map);
        _ ->
            maps:put(K, NewCounts, Map)
    end.
