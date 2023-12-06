-module(aoc_2023_6_2).

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
            Time = kerning(L),
            Distance = kerning(io:get_line("")),
            {Time, Distance}
    end.

kerning(L) ->
    kerning_impl(0, L).

kerning_impl(Acc, []) ->
    Acc;
kerning_impl(Acc, [H | T]) when H >= $0, H =< $9 ->
    kerning_impl(Acc * 10 + H - $0, T);
kerning_impl(Acc, [_ | T]) ->
    kerning_impl(Acc, T).

ini() ->
    0.

do({Time, Distance}) ->
    ways(Time, Distance).

ways(Time, Distance) ->
    F = fun(Hold) -> Hold * (Time - Hold) > Distance end,
    length(lists:filter(F, lists:seq(0, Time))).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
