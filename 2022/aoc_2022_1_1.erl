-module(aoc_2022_1_1).

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
        "\n" ->
            -1;
        L ->
            list_to_integer(string:trim(L))
    end.

ini() ->
    [-1].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    F = fun(-1, {MaxAcc, SumAcc}) ->
                {max(MaxAcc, SumAcc), 0};
           (N, {MaxAcc, SumAcc}) ->
                {MaxAcc, SumAcc + N}
        end,
    lists:foldl(F, {0, 0}, X).
