-module(aoc_2024_11_2).

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
            string:lexemes(L, " \n")
    end.

ini() ->
    0.

-define(BLINKS, 75).
-define(TAB, memo).

do(L) ->
    ets:new(?TAB, [named_table]),
    lists:sum([dp(X, ?BLINKS) || X <- L]).

dp(S, 1) ->
    2 - (length(S) rem 2);
dp(S, N) ->
    case ets:lookup_element(?TAB, {S, N}, 2, undefined) of
        undefined ->
            Res = blink(S, N),
%%            io:format("dp(~p,~p) = ~p~n", [S, N, Res]),
            ets:insert(?TAB, {{S, N}, Res}),
            Res;
        Res ->
            Res
    end.

blink("0", N) ->
    dp("1", N - 1);
blink(S, N) when length(S) rem 2 =:= 0 ->
    {Left, Right} = lists:split(length(S) div 2, S),
    dp(Left, N - 1) + dp(integer_to_list(list_to_integer(Right)), N - 1);
blink(S, N) ->
    dp(integer_to_list(list_to_integer(S) * 2024), N - 1).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
