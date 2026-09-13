-module(aoc_2025_6_2).

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
            array:from_list(string:chomp(Line))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Graph) ->
    fin_impl(0, [], array:size(array:get(0, Graph)) - 1, Graph).

fin_impl(Acc, [], I, _) when I < 0 ->
    Acc;
fin_impl(Acc, Xs, I, Graph) ->
    %%io:format("col(~w) = ~w~n", [I, col(I, Graph)]),
    case col(I, Graph) of
        {X, '+'} ->
            fin_impl(Acc + lists:sum([X | Xs]), [], I - 2, Graph);
        {X, '*'} ->
            F = fun(N, Y) -> N * Y end,
            fin_impl(Acc + lists:foldl(F, 1, [X | Xs]), [], I - 2, Graph);
        X ->
            fin_impl(Acc, [X | Xs], I - 1, Graph)
    end.

-define(SPACE, 16#20).

col(I, Graph) ->
    F = fun(_, Arr, Acc) ->
                case array:get(I, Arr) of
                    $+ ->
                        {Acc, '+'};
                    $* ->
                        {Acc, '*'};
                    ?SPACE ->
                        Acc;
                    C ->
                        Acc * 10 + C - $0
                end
        end,
    array:foldl(F, 0, Graph).
