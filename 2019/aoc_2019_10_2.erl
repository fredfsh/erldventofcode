-module(aoc_2019_10_2).

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
    OB = ob(Points),
    Tangents = tangents(OB, Points),
    F = fun(P) when P =:= OB ->
                false;
           (P) ->
                Tangent = tangent(OB, P),
                Round = index(P, maps:get(Tangent, Tangents)),
                {true, {Round, Tangent, P}}
        end,
    L = lists:filtermap(F, Points),
    G = fun({R, T1, _}, {R, T2, _}) ->
                case {norm(T1), norm(T2)} of
                    {{Q, {X1, Y1}}, {Q, {X2, Y2}}} ->
                        X2 * Y1 < X1 * Y2;
                    {{Q1, _}, {Q2, _}} ->
                        Q1 < Q2
                end;
           ({R1, _, _}, {R2, _, _}) ->
                R1 < R2
        end,
    {_, _, {X, Y}} = lists:nth(200, lists:sort(G, L)),
    100 * X + Y.

ob(Points) ->
    {_, P} = lists:last(lists:sort([{visible(P, Points), P} || P <- Points])),
    P.

visible(Point, Points) ->
    F = fun(P, Acc) when P =:= Point ->
                Acc;
           (P, Acc) ->
                sets:add_element(tangent(P, Point), Acc)
        end,
    sets:size(lists:foldl(F, sets:new(), Points)).

tangent({X0, Y0}, {X, Y}) ->
    tangent(X - X0, Y - Y0);
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

tangents({X0, Y0} = OB, Points) ->
    F = fun(Point, Acc) when Point =:= OB ->
                Acc;
           (Point, Acc) ->
                Tangent = tangent(OB, Point),
                maps:update_with(Tangent, fun(V) -> [Point | V] end, [Point], Acc)
        end,
    H = fun({X1, Y1}, {X2, Y2}) ->
                abs(X1 - X0) + abs(Y1 - Y0) < abs(X2 - X0) + abs(Y2 - Y0)
        end,
    G = fun(_, L) -> lists:sort(H, L) end,
    maps:map(G, lists:foldl(F, maps:new(), Points)).

index(X, [X | _]) ->
    1;
index(X, [_ | T]) ->
    1 + index(X, T).

%%        |
%%     4  |  1
%%  ------+------> x
%%     3  |  2
%%        |
%%        v y
norm({X, Y}) when X >= 0, Y < 0 ->
    {1, {-Y, X}};
norm({X, Y}) when X > 0, Y >= 0 ->
    {2, {X, Y}};
norm({X, Y}) when X =< 0, Y > 0 ->
    {3, {Y, -X}};
norm({X, Y}) when X < 0, Y =< 0 ->
    {4, {-X, -Y}}.
