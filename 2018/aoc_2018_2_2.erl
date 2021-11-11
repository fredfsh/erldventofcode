-module(aoc_2018_2_2).

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
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    [Res] = [id(A, B) || A <- X, B <- X, A < B, correct(A, B)],
    Res.

correct(A, B) when length(A) =/= length(B) ->
    false;
correct(A, B) ->
    correct_impl(A, B, false).

correct_impl([], [], Diff) ->
    Diff;
correct_impl([H | TA], [H | TB], Diff) ->
    correct_impl(TA, TB, Diff);
correct_impl([_ | TA], [_ | TB], false) ->
    correct_impl(TA, TB, true);
correct_impl(_, _, _) ->
    false.

id(A, B) ->
    id_impl([], A, B).

id_impl(Acc, [], []) ->
    lists:reverse(Acc);
id_impl(Acc, [H | TA], [H | TB]) ->
    id_impl([H | Acc], TA, TB);
id_impl(Acc, [_ | TA], [_ | TB]) ->
    id_impl(Acc, TA, TB).
