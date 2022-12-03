-module(aoc_2022_2_1).

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
    case io:fread("", "~c ~c") of
        eof ->
            eof;
        {ok, [They, Me]} ->
            {They, Me}
    end.

ini() ->
    0.

do({They, Me}) ->
    Shape = case Me of
                "X" ->
                    1;
                "Y" ->
                    2;
                "Z" ->
                    3
            end,
    Outcome = case {They, Me} of
                  {"A", "X"} ->
                      3;
                  {"B", "Y"} ->
                      3;
                  {"C", "Z"} ->
                      3;
                  {"A", "Y"} ->
                      6;
                  {"B", "Z"} ->
                      6;
                  {"C", "X"} ->
                      6;
                  _ ->
                      0
              end,
    Shape + Outcome.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
