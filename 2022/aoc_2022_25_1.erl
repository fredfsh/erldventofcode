-module(aoc_2022_25_1).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

do(X) ->
    do_impl(0, 1, lists:reverse(X)).

do_impl(Acc, _, []) ->
    Acc;
do_impl(Acc, Base, [$= | T]) ->
    do_impl(Acc - Base * 2, Base * 5, T);
do_impl(Acc, Base, [$- | T]) ->
    do_impl(Acc - Base, Base * 5, T);
do_impl(Acc, Base, [D | T]) ->
    do_impl(Acc + Base * (D - $0), Base * 5, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    fin_impl([], X).

fin_impl(Acc, 0) ->
    Acc;
fin_impl(Acc, X) when X rem 5 =:= 4 ->
    fin_impl([$- | Acc], X div 5 + 1);
fin_impl(Acc, X) when X rem 5 =:= 3 ->
    fin_impl([$= | Acc], X div 5 + 1);
fin_impl(Acc, X) ->
    fin_impl([$0 + X rem 5 | Acc], X div 5).
