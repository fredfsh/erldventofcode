-module(aoc_2022_2_2).

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
        {ok, [They, Result]} ->
            {They, Result}
    end.

ini() ->
    0.

do({They, Result}) ->
    %% A: rock, 1
    %% B: paper, 2
    %% C: scissors, 3
    {Outcome, Shape} = case Result of
                           %% loss
                           "X" when They =:= "A" ->
                               {0, 3};
                           "X" when They =:= "B" ->
                               {0, 1};
                           "X" when They =:= "C" ->
                               {0, 2};
                           %% tie
                           "Y" when They =:= "A" ->
                               {3, 1};
                           "Y" when They =:= "B" ->
                               {3, 2};
                           "Y" when They =:= "C" ->
                               {3, 3};
                           %% win
                           "Z" when They =:= "A" ->
                               {6, 2};
                           "Z" when They =:= "B" ->
                               {6, 3};
                           "Z" when They =:= "C" ->
                               {6, 1}
                       end,
    Shape + Outcome.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
