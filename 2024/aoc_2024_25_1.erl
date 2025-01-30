-module(aoc_2024_25_1).

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
        {ok, [[Type | _]]} ->
            {Type, input_impl(Type)}
    end.

input_impl(Type) ->
    G = fun(X, T) when T =:= Type ->
                X + 1;
           (X, _) ->
                X
        end,
    F = fun(_, Acc) ->
                {ok, [L]} = io:fread("", "~s"),
                lists:zipwith(G, Acc, L)
        end,
    lists:foldl(F, [0, 0, 0, 0, 0], lists:seq(1, 6)).

ini() ->
    #{$# => [], $. => []}.

do(X) ->
    X.

acc(Acc, {Type, Unit}) ->
    maps:update_with(Type, fun(V) -> [Unit | V] end, [Unit], Acc).

fin(Map) ->
%%    io:format("~p~n", [Map]),
    Locks = maps:get($#, Map),
    Keys = maps:get($., Map),
    length([undefined || Lock <- Locks, Key <- Keys, fit(Lock, Key)]).

fit(Lock, Key) ->
    F = fun(LockLen, KeyLen) ->
                LockLen =< KeyLen
        end,
    G = fun(X) -> not X end,
    lists:filter(G, lists:zipwith(F, Lock, Key)) =:= [].
