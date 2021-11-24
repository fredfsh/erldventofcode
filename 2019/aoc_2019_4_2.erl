-module(aoc_2019_4_2).

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
    password_impl(sets:new(), undefined, 0, X).

password_impl(Set, _, N, 0) ->
    sets:is_element(2, Set) orelse N =:= 2;
password_impl(Set, Last, N, X) ->
    case X rem 10 of
        Last ->
            password_impl(Set, Last, N + 1, X div 10);
        Y when Y > Last ->
            false;
        Y ->
            password_impl(sets:add_element(N, Set), Y, 1, X div 10)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
