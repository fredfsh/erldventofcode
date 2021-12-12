-module(aoc_2021_12_2).

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
        {ok, [L]} ->
            [A, B] = string:split(L, "-"),
            {A, B}
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, {A, B}) ->
    add_edge(A, B, add_edge(B, A, Acc)).

add_edge(A, B, Map) ->
    F = fun(Set) -> sets:add_element(B, Set) end,
    maps:update_with(A, F, sets:from_list([B]), Map).

fin(X) ->
    dfs("start", sets:from_list(["start"]), undefined, X).

dfs("end", _, _, _) ->
    1;
dfs(Node, Visited, Twice, Edges) ->
    Nexts = maps:get(Node, Edges),
    F = fun(Next, Acc) ->
                case sets:is_element(Next, Visited) of
                    true when Twice =/= undefined; Next =:= "start" ->
                        Acc;
                    true ->
                        Acc + dfs(Next, Visited, Next, Edges);
                    false ->
                        NewVisited = case cave_size(Next) of
                                         small ->
                                             sets:add_element(Next, Visited);
                                         big ->
                                             Visited
                                     end,
                        Acc + dfs(Next, NewVisited, Twice, Edges)
                end
        end,
    sets:fold(F, 0, Nexts).

cave_size([H | _]) when H >= $a, H =< $z ->
    small;
cave_size([H | _]) when H >= $A, H =< $Z ->
    big.
