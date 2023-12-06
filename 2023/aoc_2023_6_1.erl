-module(aoc_2023_6_1).

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
    case io:fread("", "Time:") of
        eof ->
            eof;
        {ok, []} ->
            Times = input_times(),
            Distances = input_distance(length(Times)),
            lists:zip(Times, Distances)
    end.

input_times() ->
    input_times_impl([]).

input_times_impl(Acc) ->
    case io:fread("", "~s") of
        {ok, ["Distance:"]} ->
            lists:reverse(Acc);
        {ok, [S]} ->
            input_times_impl([list_to_integer(S) | Acc])
    end.

input_distance(N) ->
    [hd(element(2, io:fread("", "~d"))) || _ <- lists:seq(1, N)].

ini() ->
    0.

do(Races) ->
    F = fun({Time, Distance}, Acc) ->
                Acc * ways(Time, Distance)
        end,
    lists:foldl(F, 1, Races).

ways(Time, Distance) ->
    F = fun(Hold) -> Hold * (Time - Hold) > Distance end,
    length(lists:filter(F, lists:seq(0, Time))).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
