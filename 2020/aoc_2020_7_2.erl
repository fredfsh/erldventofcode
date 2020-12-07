-module(aoc_2020_7_2).

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
                [Count, C1, C2, _] = string:split(S1, " ", all),
                {N, _} = string:to_integer(Count),
                {{list_to_atom(C1), list_to_atom(C2)}, N}
        end,
    lists:map(F, Sentences).

add(Map, Outer, Inners) ->
    maps:put(Outer, Inners, Map).

do(Map) ->
    ets:new(memo, [named_table]),
    memo(Map, {shiny, gold}) - 1.

memo(Map, X) ->
    case ets:lookup(memo, X) of
        [] ->
            Inners = maps:get(X, Map),
            F = fun({Color, Count}, Acc) ->
                        N = memo(Map, Color),
                        Acc + Count * N
                end,
            Res = lists:foldl(F, 1, Inners),
            ets:insert(memo, {X, Res}),
            Res;
        [{X, N}] ->
            N
    end.
