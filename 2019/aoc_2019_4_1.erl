-module(aoc_2019_4_1).

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
    case io:fread("", "~d-~d") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    0.

do({L, R}) ->
    F = fun(X, Acc) ->
                case password(X) of
                    true ->
                        Acc + 1;
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, 0, lists:seq(L, R)).

password(X) ->
    password_impl(undefined, false, X).

password_impl(_, Double, 0) ->
    Double;
password_impl(Last, Double, X) ->
    case X rem 10 of
        Last ->
            password_impl(Last, true, X div 10);
        N when N > Last ->
            false;
        N ->
            password_impl(N, Double, X div 10)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
