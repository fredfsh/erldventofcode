-module(aoc_2025_2_2).

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
    ByLength = [invalids(Left, Right, X) || X <- lists:seq(2, length(Right))],
    List = lists:append(ByLength),
    Set = sets:from_list(List),
    %%io:format("~w~n", [[list_to_integer(X) || X <- sets:to_list(Set)]]),
    lists:sum([list_to_integer(X) || X <- sets:to_list(Set)]).

invalids(Left, Right, N) ->
    %%io:format("invalids(~s, ~s, ~w) = {~w, ~w}~n", [Left, Right, N, ge(Left, N), le(Right, N)]),
    invalids_impl(ge(Left, N), le(Right, N), N).

ge(S, N) when length(S) rem N =:= 0 ->
    Len = length(S) div N,
    {Head, _} = lists:split(Len, S),
    case lists:append(lists:duplicate(N, Head)) >= S of
        true ->
            list_to_integer(Head);
        false ->
            list_to_integer(Head) + 1
    end;
ge(S, N) ->
    trunc(math:pow(10, length(S) div N)).

le(S, N) when length(S) rem N =:= 0 ->
    Len = length(S) div N,
    {Head, _} = lists:split(Len, S),
    case lists:append(lists:duplicate(N, Head)) =< S of
        true ->
            list_to_integer(Head);
        false ->
            list_to_integer(Head) - 1
    end;
le(S, N) ->
    trunc(math:pow(10, length(S) div N) - 1).

invalids_impl(From, To, _) when From > To ->
    [];
invalids_impl(From, To, N) ->
    F = fun(X) ->
                S = integer_to_list(X),
                lists:append(lists:duplicate(N, S))
        end,
    [F(X) || X <- lists:seq(From, To)].

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
