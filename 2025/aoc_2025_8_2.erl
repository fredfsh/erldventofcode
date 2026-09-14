-module(aoc_2025_8_2).

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
    Map = maps:from_list([{P#point.id, P} || P <- Points]),
    Circuits = maps:from_keys(lists:seq(0, length(Points) - 1), 1),
    Conns = [#conn{dist = distance(P1, P2), p1 = P1#point.id, p2 = P2#point.id}
             || P1 <- Points, P2 <- Points, P1#point.id < P2#point.id],
    Connections = lists:sort(Conns),
    connect(Connections, Map, Circuits).

distance(#point{x = X1, y = Y1, z = Z1}, #point{x = X2, y = Y2, z = Z2}) ->
    math:sqrt((X1 - X2)*(X1 - X2) + (Y1 - Y2)*(Y1 - Y2) + (Z1 - Z2)*(Z1 - Z2)).

connect([#conn{p1 = ID1, p2 = ID2} | T], Points, Circuits) ->
    case {circuit(ID1, Points), circuit(ID2, Points)} of
        {C, C} ->
            connect(T, Points, Circuits);
        {C1, C2} ->
            C = min(C1, C2),
            P1 = maps:get(C1, Points),
            P2 = maps:get(C2, Points),
            NewPoints = Points#{C1 => P1#point{circuit = C},
                                C2 => P2#point{circuit = C}},
            Size1 = maps:get(C1, Circuits),
            Size2 = maps:get(C2, Circuits),
            case Size1 + Size2 =:= maps:size(Points) of
                true ->
                    #point{x = X1} = maps:get(ID1, Points),
                    #point{x = X2} = maps:get(ID2, Points),
                    X1 * X2;
                false ->
                    NewCircuits = Circuits#{C => Size1 + Size2},
                    connect(T, NewPoints, NewCircuits)
            end
    end.

circuit(ID, Points) ->
    case maps:get(ID, Points) of
        #point{circuit = ID} ->
            ID;
        #point{circuit = Circuit} ->
            circuit(Circuit, Points)
    end.
