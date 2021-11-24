-module(aoc_2019_3_1).

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
    case io:fread("", "~s ~s") of
        eof ->
            eof;
        {ok, [S1, S2]} ->
            {input_path(S1), input_path(S2)}
    end.

input_path(S) ->
    F = fun([Dir | LenS]) ->
                {Dir, list_to_integer(LenS)}
        end,
    lists:map(F, string:split(S, ",", all)).

ini() ->
    0.

do({P1, P2}) ->
    Set = walk(sets:from_list([{0, 0}]), {0, 0}, P1, 1),
    walk(Set, {0, 0}, P2, 2).

walk(Set, XY, P, Pass) ->
    walk_impl({undefined, undefined}, Set, XY, P, Pass).

walk_impl(_, Set, _, [], 1) ->
    Set;
walk_impl({Dist, _}, _, _, [], 2) ->
    Dist;
walk_impl(Min, Set, XY, [{Dir, Len} | T], Pass) ->
    Seq = lists:seq(1, Len),
    F = fun(_, {MinAcc, SetAcc, XYAcc}) ->
                NXY = step(Dir, XYAcc),
                {NMin, NSet} =
                    case sets:is_element(NXY, SetAcc) of
                        true when Pass =:= 2 ->
                            {min(MinAcc, {dist(NXY), NXY}), SetAcc};
                        false when Pass =:= 1 ->
                            {MinAcc, sets:add_element(NXY, SetAcc)};
                        _ ->
                            {MinAcc, SetAcc}
                    end,
                {NMin, NSet, NXY}
        end,
    {NewMin, NewSet, NewXY} = lists:foldl(F, {Min, Set, XY}, Seq),
    walk_impl(NewMin, NewSet, NewXY, T, Pass).

dist({X, Y}) ->
    abs(X) + abs(Y).

step($U, {X, Y}) ->
    {X, Y + 1};
step($D, {X, Y}) ->
    {X, Y - 1};
step($L, {X, Y}) ->
    {X - 1, Y};
step($R, {X, Y}) ->
    {X + 1, Y}.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
