-module(aoc_2020_7_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(maps:new()).

input_impl(Map) ->
    case io:fread("", "~a ~a bags contain") of
        eof ->
            Map;
        {ok, [C1, C2]} ->
            Outer = {C1, C2},
            Line = io:get_line(""),
            Inners = inners(Line),
            input_impl(add(Map, Outer, Inners))
    end.

inners(" no other bags.\n") ->
    [];
inners(" no other bags.") ->
    [];
inners(L) ->
    Sentences = string:split(L, ",", all),
    F = fun(S) ->
                S1 = string:trim(S, both),
                [_Count, C1, C2, _] = string:split(S1, " ", all),
                {list_to_atom(C1), list_to_atom(C2)}
        end,
    lists:map(F, Sentences).

add(Map, Outer, Inners) ->
    F = fun(Inner, Acc) ->
                Set = maps:get(Inner, Acc, sets:new()),
                case sets:is_element(Outer, Set) of
                    true ->
                        Acc;
                    _ ->
                        maps:put(Inner, sets:add_element(Outer, Set), Acc)
                end
        end,
    lists:foldl(F, Map, Inners).

do(X) ->
    Open = queue({shiny, gold}, X),
    bfs(X, Open, sets:new()).

bfs(Map, Open, Visited) ->
    case queue:out(Open) of
        {empty, _} ->
            sets:size(Visited);
        {{value, Head}, Open2} ->
            case sets:is_element(Head, Visited) of
                true ->
                    bfs(Map, Open2, Visited);
                _ ->
                    NewOpen = queue:join(Open2, queue(Head, Map)),
                    NewVisited = sets:add_element(Head, Visited),
                    bfs(Map, NewOpen, NewVisited)
            end
    end.

queue(X, Map) ->
    queue:from_list(sets:to_list(maps:get(X, Map, sets:new()))).
