-module(aoc_2019_10_1).

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
    {0, []}.

do(X) ->
    X.

acc({Y, Points}, L) ->
    F = fun($., {X, PointsAcc}) ->
                {X + 1, PointsAcc};
           ($#, {X, PointsAcc}) ->
                {X + 1, [{X, Y} | PointsAcc]}
        end,
    {_, NewPoints} = lists:foldl(F, {0, Points}, L),
    {Y + 1, NewPoints}.

fin({_, Points}) ->
    lists:last(lists:sort([visible(P, Points) || P <- Points])).

visible(Point, Points) ->
    F = fun(P, Acc) when P =:= Point ->
                Acc;
           (P, Acc) ->
                sets:add_element(tangent(P, Point), Acc)
        end,
    sets:size(lists:foldl(F, sets:new(), Points)).

tangent({X1, Y1}, {X2, Y2}) ->
    tangent(X1 - X2, Y1 - Y2);
tangent(X, Y) ->
    Z = gcd(abs(X), abs(Y)),
    {X div Z, Y div Z}.

gcd(X, X) ->
    X;
gcd(X, Y) when X < Y ->
    gcd(Y, X);
gcd(X, 0) ->
    X;
gcd(X, Y) ->
    gcd(Y, X rem Y).
