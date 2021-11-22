-module(aoc_2018_23_1).

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
    case io:fread("", "pos=<~d,~d,~d>, r=~d") of
        eof ->
            eof;
        {ok, [X, Y, Z, R]} ->
            {X, Y, Z, R}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(Bots) ->
    Strongest = lists:last(lists:keysort(4, Bots)),
    {_, _, _, R} = Strongest,
    F = fun(Bot) -> dist(Bot, Strongest) =< R end,
    length(lists:filter(F, Bots)).

dist({X1, Y1, Z1, _}, {X2, Y2, Z2, _}) ->
    abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2).
