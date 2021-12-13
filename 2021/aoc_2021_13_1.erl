-module(aoc_2021_13_1).

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
    case io:fread("", "~a") of
        eof ->
            eof;
        {ok, [fold]} ->
            {ok, [Z, N]} = io:fread("", " along ~c=~d"),
            {Z, N};
        {ok, [A]} ->
            [SX, SY] = string:split(atom_to_list(A), ","),
            {list_to_integer(SX), list_to_integer(SY)}
    end.

ini() ->
    {sets:new(), []}.

do(X) ->
    X.

acc(Acc, []) ->
    Acc;
acc({Points, Folds}, {Z, N}) when is_list(Z) ->
    {Points, [{Z, N} | Folds]};
acc({Points, Folds}, {X, Y}) ->
    {sets:add_element({X, Y}, Points), Folds}.

fin({Points, Folds}) ->
    sets:size(fold(Points, lists:last(Folds))).

fold(Points, {Z, N}) ->
    F = fun({X, Y}, Acc)
              when (Z =:= "x" andalso X < N) orelse (Z =:= "y" andalso Y < N) ->
                Acc;
           ({X, Y}, Acc) when Z =:= "x" ->
                sets:add_element({2 * N - X, Y}, sets:del_element({X, Y}, Acc));
           ({X, Y}, Acc) when Z =:= "y" ->
                sets:add_element({X, 2 * N - Y}, sets:del_element({X, Y}, Acc))
        end,
    sets:fold(F, Points, Points).
