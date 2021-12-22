-module(aoc_2021_22_1).

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
    case io:fread("", "~a x=~d..~d,y=~d..~d,z=~d..~d") of
        eof ->
            eof;
        {ok, L} ->
            L
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(L) ->
    run(sets:new(), lists:reverse(L)).

-define(N, 50).

run(Ons, []) ->
    sets:size(Ons);
run(Ons, [[_, X1, X2, Y1, Y2, Z1, Z2] | T])
  when X2 < -?N; X1 > ?N; Y2 < -?N; Y1 > ?N; Z2 < -?N; Z1 > ?N ->
    run(Ons, T);
run(Ons, [H | T]) ->
    run(act(Ons, H), T).

act(Ons, [on, X1, X2, Y1, Y2, Z1, Z2]) ->
    L = [{X, Y, Z} || X <- lists:seq(X1, X2),
                      Y <- lists:seq(Y1, Y2),
                      Z <- lists:seq(Z1, Z2)],
    F = fun({X, Y, Z}, Acc) ->
                sets:add_element({X, Y, Z}, Acc)
        end,
    lists:foldl(F, Ons, L);
act(Ons, [off, X1, X2, Y1, Y2, Z1, Z2]) ->
    L = [{X, Y, Z} || X <- lists:seq(X1, X2),
                      Y <- lists:seq(Y1, Y2),
                      Z <- lists:seq(Z1, Z2)],
    F = fun({X, Y, Z}, Acc) ->
                sets:del_element({X, Y, Z}, Acc)
        end,
    lists:foldl(F, Ons, L).
