-module(aoc_2024_7_1).

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
            Strs = string:lexemes(string:trim(L, trailing), ": "),
            [Val | Ops] = [list_to_integer(S) || S <- Strs],
            {Val, Ops}
    end.

ini() ->
    0.

do({Val, Ops}) ->
    case eq(Val, Ops) of
        true ->
            Val;
        false ->
            0
    end.

eq(Val, Ops) ->
    eq_impl(Val, lists:reverse(Ops)).

eq_impl(X, [Y]) ->
    X =:= Y;
eq_impl(X, [H | _]) when X < H ->
    false;
eq_impl(X, [H | T]) ->
    eq_impl(X - H, T) orelse (X rem H =:= 0 andalso eq_impl(X div H, T)).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
