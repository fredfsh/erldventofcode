-module(aoc_2019_3_2).

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
    Init = {{0, 0}, 0},
    Map = walk(maps:from_list([Init]), Init, P1, 1),
    walk(Map, Init, P2, 2).

walk(Map, Cur, P, Pass) ->
    walk_impl(undefined, Map, Cur, P, Pass).

walk_impl(_, Map, _, [], 1) ->
    Map;
walk_impl(Min, _, _, [], 2) ->
    Min;
walk_impl(Min, Map, State, [{Dir, Len} | T], Pass) ->
    Seq = lists:seq(1, Len),
    F = fun(_, {MinAcc, MapAcc, {XYAcc, NAcc}}) ->
                NXY = step(Dir, XYAcc),
                {NMin, NMap} =
                    case maps:get(NXY, MapAcc, undefined) of
                        undefined when Pass =:= 1 ->
                            {MinAcc, maps:put(NXY, NAcc + 1, MapAcc)};
                        undefined ->
                            {MinAcc, MapAcc};
                        N ->
                            {min(MinAcc, N + NAcc + 1), MapAcc}
                    end,
                {NMin, NMap, {NXY, NAcc + 1}}
        end,
    {NewMin, NewMap, NewState} = lists:foldl(F, {Min, Map, State}, Seq),
    walk_impl(NewMin, NewMap, NewState, T, Pass).

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
