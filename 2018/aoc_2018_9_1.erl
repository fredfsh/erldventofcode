-module(aoc_2018_9_1).

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
    sim(0, Players, 0, [0], maps:new(), Players, Marbles).

sim(Marbles, _, _, _, Scores, _, Marbles) ->
    lists:last(lists:sort(maps:values(Scores)));
sim(Marble, Player, Cur, L, Scores, Players, Marbles) ->
    NextPlayer = (Player + 1) rem Players,
    {NewCur, NewL, NewScores} =
        case Marble + 1 of
            N when N rem 23 =:= 0 ->
                Scores2 = score(Scores, N, NextPlayer),
                Index = (Cur - 7 + length(L)) rem length(L),
                Removed = lists:nth(Index + 1, L),
                {Index rem (length(L) - 1),
                 lists:delete(Removed, L),
                 score(Scores2, Removed, NextPlayer)};
            N ->
                Index = (Cur + 1) rem length(L),
                {Left, Right} = lists:split(Index + 1, L),
                {Index + 1,
                 lists:append([Left, [N], Right]),
                 Scores}
        end,
    sim(Marble + 1, NextPlayer, NewCur, NewL, NewScores, Players, Marbles).

score(Scores, Score, Player) ->
    maps:update_with(Player, fun(V) -> V + Score end, Score, Scores).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
