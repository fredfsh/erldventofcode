-module(aoc_2016_16_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    {ok, [X]} = io:fread("", "~s"),
    X.

do(X) ->
    checksum(data(X)).

-define(LENGTH, 35651584).

data(X) when length(X) >= ?LENGTH ->
    string:slice(X, 0, ?LENGTH);
data(X) ->
    data(expand(X)).

expand(X) ->
    X ++ [$0] ++ [$0 + $1 - A || A <- lists:reverse(X)].

checksum(X) when length(X) rem 2 =:= 1 ->
    X;
checksum(X) ->
    checksum(shrink(X, [])).

shrink([], Acc) ->
    lists:reverse(Acc);
shrink([X, X | T], Acc) ->
    shrink(T, [$1 | Acc]);
shrink([_, _ | T], Acc) ->
    shrink(T, [$0 | Acc]).
