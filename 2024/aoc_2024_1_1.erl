-module(aoc_2024_1_1).

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
    F = fun(A, B) -> erlang:abs(A - B) end,
    lists:sum(lists:zipwith(F, lists:sort(LA), lists:sort(LB))).
