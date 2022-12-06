-module(aoc_2022_6_1).

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
    do_impl(0, X).

do_impl(I, [A, B, C, D | _]) when A =/= B andalso A =/= C andalso A =/= D andalso
                                  B =/= C andalso B =/= D andalso
                                  C =/= D ->
    I + 4;
do_impl(I, [_ | T]) ->
    do_impl(I + 1, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
