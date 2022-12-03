-module(aoc_2022_3_2).

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
            string:trim(L)
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    fin_impl(0, X).

fin_impl(Res, []) ->
    Res;
fin_impl(Acc, [A, B, C | T]) ->
    fin_impl(Acc + priority(A, B, C), T).

priority(A, B, C) ->
    Badges = sets:from_list(A),
    Badges2 = sets:intersection(Badges, sets:from_list(B)),
    Badges3 = sets:intersection(Badges2, sets:from_list(C)),
    [X] = sets:to_list(Badges3),
    case X =< $z andalso X >= $a of
        true ->
            X - $a + 1;
        false ->
            X - $A + 27
    end.
