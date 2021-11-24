-module(aoc_2019_6_2).

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
            string:split(X, ")")
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, [Inner, Outer]) ->
    maps:put(Outer, Inner, Acc).

fin(X) ->
    You = maps:get("YOU", X),
    San = maps:get("SAN", X),
    dist(You, San, X).

dist(A, B, Parents) ->
    PA = path(A, Parents),
    PB = path(B, Parents),
    Common = common(PA, PB),
    (length(PA) - length(Common)) + (length(PB) - length(Common)).

path(X, Parents) ->
    path_impl([X], X, Parents).

path_impl(Acc, Node, Parents) ->
    case maps:get(Node, Parents, undefined) of
        undefined ->
            Acc;
        Parent ->
            path_impl([Parent | Acc], Parent, Parents)
    end.

common(A, B) ->
    common_impl([], A, B).

common_impl(Acc, [], _) ->
    Acc;
common_impl(Acc, _, []) ->
    Acc;
common_impl(Acc, [H | TA], [H | TB]) ->
    common_impl([H | Acc], TA, TB);
common_impl(Acc, _, _) ->
    Acc.
