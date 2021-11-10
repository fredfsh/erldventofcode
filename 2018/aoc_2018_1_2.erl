-module(aoc_2018_1_2).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    L = lists:reverse(X),
    twice(0, sets:from_list([0]), L, L).

twice(Frequency, Seen, [], L) ->
    twice(Frequency, Seen, L, L);
twice(Frequency, Seen, [H | T], L) ->
    Next = Frequency + H,
    case sets:is_element(Next, Seen) of
        true ->
            Next;
        false ->
            twice(Next, sets:add_element(Next, Seen), T, L)
    end.
