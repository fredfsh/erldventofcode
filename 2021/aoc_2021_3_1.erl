-module(aoc_2021_3_1).

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
        {ok, [L]} ->
            [X - $0 ||  X <- L]
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(L) ->
    N = length(hd(L)),
    L0 = lists:duplicate(N, 0),
    F = fun(BinL, Acc) ->
                add(BinL, Acc)
        end,
    L2 = lists:foldl(F, L0, L),
    L3 = [case X >= length(L) div 2 of
              true ->
                  1;
              false ->
                  0
          end || X <- L2],
    L4 = [X + $0 || X <- L3],
    L5 = [$0 + $1 - X  || X <- L4],
    list_to_integer(L4, 2) * list_to_integer(L5, 2).

add(L1, L2) ->
    lists:zipwith(fun(X, Y) -> X + Y end, L1, L2).
