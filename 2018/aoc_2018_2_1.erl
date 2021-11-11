-module(aoc_2018_2_1).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    {0, 0}.

do(X) ->
    F = fun(C, Acc) ->
                maps:update_with(C, fun(V) -> V + 1 end, 1, Acc)
        end,
    Counts = lists:foldl(F, maps:new(), X),
    L = maps:to_list(Counts),
    Two = case lists:keyfind(2, 2, L) of
              false ->
                  0;
              _ ->
                  1
          end,
    Three = case lists:keyfind(3, 2, L) of
                false ->
                    0;
                _ ->
                    1
            end,
    {Two, Three}.

acc({Twos, Threes}, {Two, Three}) ->
    {Twos + Two, Threes + Three}.

fin({Twos, Threes}) ->
    Twos * Threes.
