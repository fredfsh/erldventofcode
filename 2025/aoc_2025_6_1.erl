-module(aoc_2025_6_1).

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
            Parts = string:lexemes(string:chomp(Line), " "),
            case lists:member("+", Parts) orelse lists:member("*", Parts) of
                true ->
                    Parts;
                false ->
                    [list_to_integer(X) || X <- Parts]
            end
    end.

ini() ->
    undefined.

do(X) ->
    X.

acc(undefined, L) ->
    [[X] || X <- L];
acc(Acc, L) when is_integer(hd(L)) ->
    F = fun(Xs, X) -> [X | Xs] end,
    lists:zipwith(F, Acc, L);
acc(Acc, Ops) ->
    F = fun(Xs, "+") ->
                lists:sum(Xs);
           (Xs, "*") ->
                lists:foldl(fun(X, XAcc) -> X * XAcc end, 1, Xs)
        end,
    lists:sum(lists:zipwith(F, Acc, Ops)).

fin(X) ->
    X.
