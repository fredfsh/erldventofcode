-module(aoc_2024_1_2).

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
    case io:fread("", "~d ~d") of
        eof ->
            eof;
        {ok, [A, B]} ->
            {A, B}
    end.

ini() ->
    {[], []}.

do(X) ->
    X.

acc({LA, LB}, {A, B}) ->
    {[A | LA], [B | LB]}.

fin({LA, LB}) ->
    lists:sum([X * counts(X, LB) || X <- LA]).

counts(X, L) ->
    length([undefined || Y <- L, Y =:= X]).
