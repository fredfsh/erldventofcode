-module(aoc_2019_12_1).

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
    case io:fread("", "<x=~d, y=~d, z=~d>") of
        eof ->
            eof;
        {ok, [X, Y, Z]} ->
            {X, Y, Z}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(Moons) ->
    sim([{X, Y, Z, 0, 0, 0} || {X, Y, Z} <- Moons]).

sim(Moons) ->
    sim_impl(0, Moons).

-define(TIMES, 1000).

sim_impl(?TIMES, Moons) ->
    energy(Moons);
sim_impl(I, Moons) ->
    F = fun({X0, Y0, Z0, VX, VY, VZ}) ->
                G = fun({X, Y, Z, _, _, _}, {VXAcc, VYAcc, VZAcc}) ->
                            {pull(VXAcc, X, X0),
                             pull(VYAcc, Y, Y0),
                             pull(VZAcc, Z, Z0)}
                    end,
                {NVX, NVY, NVZ} = lists:foldl(G, {VX, VY, VZ}, Moons),
                {X0 + NVX, Y0 + NVY, Z0 + NVZ, NVX, NVY, NVZ}
        end,
    sim_impl(I + 1, lists:map(F, Moons)).

energy(Moons) ->
    lists:sum([(abs(X) + abs(Y) + abs(Z)) * (abs(VX) + abs(VY) + abs(VZ))
               || {X, Y, Z, VX, VY, VZ} <- Moons]).

pull(V, X0, X0) ->
    V;
pull(V, X, X0) when X > X0 ->
    V + 1;
pull(V, X, X0) when X < X0 ->
    V - 1.
