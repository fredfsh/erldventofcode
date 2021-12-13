-module(aoc_2021_13_2).

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
    fold(Points, lists:reverse(Folds)).

fold(Points, Folds) ->
    Ps = lists:foldl(fun(Fold, Acc) -> fold_impl(Fold, Acc) end, Points, Folds),
    print(Ps).

fold_impl({Z, N}, Points) ->
    F = fun({X, Y}, Acc)
              when (Z =:= "x" andalso X < N) orelse (Z =:= "y" andalso Y < N) ->
                Acc;
           ({X, Y}, Acc) when Z =:= "x" ->
                sets:add_element({2 * N - X, Y}, sets:del_element({X, Y}, Acc));
           ({X, Y}, Acc) when Z =:= "y" ->
                sets:add_element({X, 2 * N - Y}, sets:del_element({X, Y}, Acc))
        end,
    sets:fold(F, Points, Points).

print(Ps) ->
    {Xs, Ys} = lists:unzip(sets:to_list(Ps)),
    MinX = lists:min(Xs),
    MaxX = lists:max(Xs),
    MinY = lists:min(Ys),
    MaxY = lists:max(Ys),
    G = fun(Y) ->
                L = [case sets:is_element({X, Y}, Ps) of
                         true ->
                             $#;
                         false ->
                             $.
                     end || X <- lists:seq(MinX, MaxX)],
                io:format("~s~n", [L])
        end,
    lists:foreach(G, lists:seq(MinY, MaxY)).
