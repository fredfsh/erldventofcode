-module(aoc_2024_2_1).

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
    case io:get_line("") of
        eof ->
            eof;
        L ->
            Strs = string:split(string:trim(L), " ", all),
            [list_to_integer(S) || S <- Strs]
    end.

ini() ->
    0.

do(X) ->
    do_impl(X).

do_impl([H1, H2, H3 | T]) ->
    S1 = (H2 - H1) * (H3 - H2),
    S2 = (H2 - H1) * (H2 - H1) + (H3 - H2) * (H3 - H2),
    case S1 > 0 andalso S2 =< 18 andalso S2 =/= 17 of
        true ->
            do_impl([H2, H3 | T]);
        false ->
            0
    end;
do_impl(_) ->
    1.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
