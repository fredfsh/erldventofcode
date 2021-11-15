-module(aoc_2018_9_2).

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
    case io:fread("", "~d players; last marble is worth ~d points") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    0.

do({Players, Marbles}) ->
    Map = maps:from_list([{0, {1, 0, 2}}, {2, {0, 2, 1}}, {1, {2, 1, 0}}]),
    sim(2, Players, 2, {3, Map}, maps:new(), Players, Marbles * 100).

sim(Marbles, _, _, _, Scores, _, Marbles) ->
    lists:last(lists:sort(maps:values(Scores)));
sim(Marble, Player, Cur, List, Scores, Players, Marbles) ->
    NextPlayer = (Player + 1) rem Players,
    {NewCur, NewList, NewScores} =
        case Marble + 1 of
            N when N rem 23 =:= 0 ->
                Scores2 = score(Scores, N, NextPlayer),
                Index = prev(List, Cur, 7),
                Scores3 = score(Scores2, val(List, Index), NextPlayer),
                {next(List, Index), delete(List, Index), Scores3};
            N ->
                Index = next(List, Cur),
                List2 = insert(List, Index, N),
                {next(List2, Index), List2, Scores}
        end,
    sim(Marble + 1, NextPlayer, NewCur, NewList, NewScores, Players, Marbles).

score(Scores, Score, Player) ->
    maps:update_with(Player, fun(V) -> V + Score end, Score, Scores).

prev(_, Cur, 0) ->
    Cur;
prev({_, Map} = List, Cur, N) ->
    {Prev, _, _} = maps:get(Cur, Map),
    prev(List, Prev, N - 1).

val({_, Map}, Cur) ->
    {_, Val, _} = maps:get(Cur, Map),
    Val.

next({_, Map}, Cur) ->
    {_, _, Next} = maps:get(Cur, Map),
    Next.

delete({I, Map}, Cur) ->
    {Prev, _, Next} = maps:get(Cur, Map),
    {PPrev, PVal, Cur} = maps:get(Prev, Map),
    {Cur, NVal, NNext} = maps:get(Next, Map),
    Map2 = maps:put(Prev, {PPrev, PVal, Next}, Map),
    Map3 = maps:put(Next, {Prev, NVal, NNext}, Map2),
    {I, Map3}.

insert({I, Map}, Cur, Value) ->
    {Prev, Val, Next} = maps:get(Cur, Map),
    {Cur, NVal, NNext} = maps:get(Next, Map),
    Map2 = maps:put(I, {Cur, Value, Next}, Map),
    Map3 = maps:put(Cur, {Prev, Val, I}, Map2),
    Map4 = maps:put(Next, {I, NVal, NNext}, Map3),
    {I + 1, Map4}.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
