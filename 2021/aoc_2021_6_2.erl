-module(aoc_2021_6_2).

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
            [list_to_integer(X) || X <- string:split(L, ",", all)]
    end.

ini() ->
    0.

-define(DAYS, 256).
-define(TAB, memo).

do(L) ->
    ets:new(?TAB, [named_table]),
    lists:sum([dp(X, ?DAYS) || X <- L]).

dp(X, D) ->
    case ets:lookup(?TAB, {X, D}) of
        [] ->
            Res = dp_impl(X, D),
            ets:insert_new(?TAB, {{X, D}, Res}),
            Res;
        [{_, Res}] ->
            Res
    end.

dp_impl(_, 0) ->
    1;
dp_impl(0, D) ->
    dp(6, D - 1) + dp(8, D - 1);
dp_impl(X, D) ->
    dp(X - 1, D - 1).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
