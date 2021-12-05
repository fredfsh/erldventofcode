-module(aoc_2019_22_1).

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
    case io:fread("", "~a") of
        eof ->
            eof;
        {ok, [cut]} ->
            {ok, [X]} = io:fread("", "~d"),
            {cut, X};
        {ok, [deal]} ->
            case io:fread("", "~a") of
                {ok, [with]} ->
                    {ok, [_, X]} = io:fread("", "~s ~d"),
                    {deal, X};
                {ok, [into]} ->
                    io:get_line(""),
                    reverse
            end
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

-define(N, 10007).

-define(TAB, memo).

fin(X) ->
    ets:new(?TAB, [named_table]),
    shuffle(lists:seq(0, ?N - 1), 0, 1, lists:reverse(X)).

-define(TARGET, 2019).
-define(AT(I), lists:nth((Top + D * I + ?N) rem ?N + 1, Deck)).

shuffle(Deck, Top, D, []) ->
    index_of(?TARGET, Deck, Top, D);
shuffle(Deck, Top, D, [reverse | T]) ->
    shuffle(Deck, (Top + ?N - D) rem ?N, -D, T);
shuffle(Deck, Top, D, [{cut, X} | T]) ->
    shuffle(Deck, (Top + X * D + ?N) rem ?N, D, T);
shuffle(Deck, Top, D, [{deal, X} | T]) ->
    shuffle(deal(X, Deck, Top, D), 0, 1, T).

index_of(X, Deck, Top, D) ->
    index_of_impl(0, X, Deck, Top, D).

index_of_impl(I, X, Deck, Top, D) ->
    case ?AT(I) of
        X ->
            I;
        _ ->
            index_of_impl(I + 1, X, Deck, Top, D)
    end.

deal(X, Deck, Top, D) ->
    [?AT(index(I, X))|| I <- lists:seq(0, ?N - 1)].

index(I, X) ->
    case ets:lookup(?TAB, {I, X}) of
        [] ->
            Index = index_impl(I, X),
            ets:insert_new(?TAB, {{I, X}, Index}),
            Index;
        [{_, Index}] ->
            Index
    end.

index_impl(I, X) when I rem X =:= 0 ->
    I div X;
index_impl(I, X) ->
    index_impl(I + ?N, X).
