-module(aoc_2022_1_2).

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
    F = fun(-1, {MaxAcc, SecondAcc, _, SumAcc}) when SumAcc >= MaxAcc ->
                {SumAcc, MaxAcc, SecondAcc, 0};
           (-1, {MaxAcc, SecondAcc, _, SumAcc}) when SumAcc >= SecondAcc ->
                {MaxAcc, SumAcc, SecondAcc, 0};
           (-1, {MaxAcc, SecondAcc, ThirdAcc, SumAcc}) when SumAcc >= ThirdAcc ->
                {MaxAcc, SecondAcc, SumAcc, 0};
           (-1, {MaxAcc, SecondAcc, ThirdAcc, _}) ->
                {MaxAcc, SecondAcc, ThirdAcc, 0};
           (N, {MaxAcc, SecondAcc, ThirdAcc, SumAcc}) ->
                {MaxAcc, SecondAcc, ThirdAcc, SumAcc + N}
        end,
    {Max, Second, Third, _} = lists:foldl(F, {0, -1, -2, 0}, X),
    Max + Second + Third.
