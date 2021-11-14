-module(aoc_2018_6_2).

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
    case io:fread("", "~d, ~d") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    {0, []}.

do(X) ->
    X.

acc({I, L}, {X, Y}) ->
    {I + 1, [{I, X, Y} | L]}.

fin({_, Inits}) ->
    {MinX, MinY, MaxX, MaxY} = borders(Inits),
    L = [within(X, Y, Inits) || X <- lists:seq(MinX, MaxX),
                                Y <- lists:seq(MinY, MaxY)],
    lists:sum(L).

borders(L) ->
    F = fun({_, X, Y}, {MinX, MinY, MaxX, MaxY}) ->
                {min(X, MinX), min(Y, MinY), max(X, MaxX), max(Y, MaxY)}
        end,
    lists:foldl(F, {undefined, undefined, -1, -1}, L).

-define(LIMIT, 10000).

within(X, Y, Inits) ->
    case lists:sum([abs(X - XX) + abs(Y - YY) || {_, XX, YY} <- Inits]) of
        N when N < ?LIMIT ->
            1;
        _ ->
            0
    end.
