-module(aoc_2019_2_1).

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
            L = [list_to_integer(Y) || Y <- string:split(X, ",", all)],
            array:from_list(L)
    end.

ini() ->
    0.

do(X) ->
    run(0, array:set(2, 2, array:set(1, 12, X))).

-define(V(X), array:get(X, Code)).

run(IP, Code) ->
    case ?V(IP) of
        99 ->
            array:get(0, Code);
        1 ->
            NewCode = array:set(?V(IP+3), ?V(?V(IP+1)) + ?V(?V(IP+2)), Code),
            run(IP + 4, NewCode);
        2 ->
            NewCode = array:set(?V(IP+3), ?V(?V(IP+1)) * ?V(?V(IP+2)), Code),
            run(IP + 4, NewCode)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
