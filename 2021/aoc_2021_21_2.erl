-module(aoc_2021_21_2).

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
    case io:fread("", "Player 1 starting position: ~d") of
        eof ->
            eof;
        {ok, [One]} ->
            {ok, [Two]} = io:fread("", "Player 2 starting position: ~d"),
            {One, Two}
    end.

ini() ->
    0.

-define(TAB, memo).

do({One, Two}) ->
    ets:new(?TAB, [named_table]),
    {Win1, Win2} = win(One, 0, Two, 0),
    max(Win1, Win2).

win(_P1, S1, _P2, _S2) when S1 >= 21 ->
    {1, 0};
win(_P1, _S1, _P2, S2) when S2 >= 21 ->
    {0, 1};
win(P1, S1, P2, S2) ->
    case ets:lookup(?TAB, {P1, S1, P2, S2}) of
        [{_, Res}] ->
            Res;
        [] ->
            Res = win_impl(P1, S1, P2, S2),
            ets:insert_new(?TAB, {{P1, S1, P2, S2}, Res}),
            Res
    end.

-define(R, [{3, 1}, {4, 3}, {5, 6}, {6, 7}, {7, 6}, {8, 3}, {9, 1}]).

win_impl(P1, S1, P2, S2) ->
    F = fun({Roll, Times}, {WinAcc, LossAcc}) ->
                NP = inc(P1, Roll),
                {Loss, Win} = win(P2, S2, NP, S1 + NP),
                {WinAcc + Win * Times, LossAcc + Loss * Times}
        end,
    lists:foldl(F, {0, 0}, ?R).

inc(P, Roll) ->
    (P + Roll - 1) rem 10 + 1.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
