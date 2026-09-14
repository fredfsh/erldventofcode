-module(aoc_2025_8_1).

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
    case io:fread("", "~d,~d,~d") of
        eof ->
            eof;
        {ok, [X, Y, Z]} ->
            {X, Y, Z}
    end.

ini() ->
    [].

do(X) ->
    X.

-record(point, {id, x, y, z, circuit}).

acc(Acc, {X, Y, Z}) ->
    ID = length(Acc),
    [#point{id = ID, x = X, y = Y, z = Z, circuit = ID} | Acc].

-record(conn, {dist, p1, p2}).

-define(CONNECTS, 1000).

fin(Points) ->
    %%io:format("points:~n~w~n", [Points]),
    Conns = [#conn{dist = distance(P1, P2), p1 = P1#point.id, p2 = P2#point.id}
             || P1 <- Points, P2 <- Points, P1#point.id < P2#point.id],
    Sorted = lists:sort(Conns),
    {Connections, _} = lists:split(?CONNECTS, Sorted),
    Connected = connect(Points, Connections),
    %%io:format("connected:~n~w~n", [Connected]),
    Circuits = circuits(Connected),
    %%io:format("circuits:~n~w~n", [Circuits]),
    Proplists = maps:to_list(Circuits),
    Asc = lists:keysort(2, Proplists),
    [{_, Size1}, {_, Size2}, {_, Size3} | _] = lists:reverse(Asc),
    Size1 * Size2 * Size3.

distance(#point{x = X1, y = Y1, z = Z1}, #point{x = X2, y = Y2, z = Z2}) ->
    math:sqrt((X1 - X2)*(X1 - X2) + (Y1 - Y2)*(Y1 - Y2) + (Z1 - Z2)*(Z1 - Z2)).

connect(Points, Connections) ->
    Map = maps:from_list([{P#point.id, P} || P <- Points]),
    F = fun(#conn{p1 = ID1, p2 = ID2}, Acc) ->
                %%io:format("map: ~w~n", [Acc]),
                C1 = circuit(ID1, Acc),
                C2 = circuit(ID2, Acc),
                Circuit = min(C1, C2),
                %%io:format("connecting ~w (~w) and ~w (~w)~n", [ID1, C1, ID2, C2]),
                P1 = maps:get(C1, Acc),
                P2 = maps:get(C2, Acc),
                Acc#{C1 => P1#point{circuit = Circuit},
                     C2 => P2#point{circuit = Circuit}}
        end,
    lists:foldl(F, Map, Connections).

circuits(Map) ->
    F = fun(ID, _, Acc) ->
                Circuit = circuit(ID, Map),
                Acc#{Circuit => maps:get(Circuit, Acc, 0) + 1}
        end,
    maps:fold(F, #{}, Map).

circuit(ID, Map) ->
    case maps:get(ID, Map) of
        #point{circuit = ID} ->
            ID;
        #point{circuit = Circuit} ->
            circuit(Circuit, Map)
    end.
