-module(aoc_2025_2_1).

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
        Line ->
            S = string:trim(Line),
            Parts = string:split(S, ",", all),
            [string:split(P, "-") || P <- Parts]
    end.

ini() ->
    0.

do(L) ->
    lists:sum([do_impl(X) || X <- L]).

do_impl([Left, Right]) ->
    case {ge(Left), le(Right)} of
        {From, To} when From > To ->
            %%io:format("ge(~w) = ~w, le(~w) = ~w~n", [Left, From, Right, To]),
            0;
        {From, To} ->
            sum(From, To)
    end.

ge(S) when length(S) rem 2 =:= 0 ->
    case lists:split(length(S) div 2, S) of
        {Left, Right} when Left >= Right ->
            list_to_integer(Left);
        {Left, _} ->
            list_to_integer(Left) + 1
    end;
ge(S) ->
    trunc(math:pow(10, length(S) div 2)).

le(S) when length(S) rem 2 =:= 0 ->
    case lists:split(length(S) div 2, S) of
        {Left, Right} when Left =< Right ->
            list_to_integer(Left);
        {Left, _} ->
            list_to_integer(Left) - 1
    end;
le(S) ->
    trunc(math:pow(10, length(S) div 2) - 1).

sum(From, To) ->
    %%io:format("~w ~w~n", [From, To]),
    F = fun(X) ->
                S = integer_to_list(X),
                list_to_integer(S ++ S)
        end,
    lists:sum([F(X) || X <- lists:seq(From, To)]).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
