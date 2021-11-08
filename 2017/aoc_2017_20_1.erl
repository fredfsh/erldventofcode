-module(aoc_2017_20_1).

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
    case io:fread("", "p=<~d,~d,~d>, v=<~d,~d,~d>, a=<~d,~d,~d>") of
        eof ->
            eof;
        {ok, [PX, PY, PZ, VX, VY, VZ, AX, AY, AZ]} ->
            {{PX, PY, PZ}, {VX, VY, VZ}, {AX, AY, AZ}}
    end.

ini() ->
    {{undefined, undefined, undefined}, undefined, 0}.

do(X) ->
    X.

acc({Min, MinI, I}, {{PX, PY, PZ}, {VX, VY, VZ}, {AX, AY, AZ}}) ->
    %% S = 0.5AT^2 + (V + 0.5A)T + P
    Dist = {abs(AX) + abs(AY) + abs(AZ),
            abs(AX + 2 * VX) + abs(AY + 2 * VY) + abs(AZ + 2 * VZ),
            abs(PX) + abs(PY) + abs(PZ)},
    case Dist < Min of
        true ->
            {Dist, I, I + 1};
        false ->
            {Min, MinI, I + 1}
    end.

fin({_, I, _}) ->
    I.
