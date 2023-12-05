-module(aoc_2023_3_2).

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
    array:new().

do(X) ->
    array:from_list(X).

acc(Matrix, Arr) ->
    array:set(array:size(Matrix), Arr, Matrix).

fin(Matrix) ->
    F = fun(Y, Arr, Acc) ->
                gears(Acc, 0, [], 0, Arr, Y, Matrix)
        end,
    {Sum, _} = array:foldl(F, {0, maps:new()}, Matrix),
    Sum.

gears(Acc, N, Stars, X, Arr, Y, Matrix) ->
    case {array:size(Arr), array:get(X, Arr)} of
        {X, _} ->
            add_gears(Acc, N, Stars);
        {_, D} when D >= $0, D =< $9 ->
            NewStars = stars(Stars, N, X, Y, Matrix),
            gears(Acc, N * 10 + (D - $0), NewStars, X + 1, Arr, Y, Matrix);
        _ when N =/= 0 ->
            NewStars = stars(Stars, N, X, Y, Matrix),
            NewAcc = add_gears(Acc, N, NewStars),
            gears(NewAcc, 0, [], X + 1, Arr, Y, Matrix);
        _ ->
            gears(Acc, 0, [], X + 1, Arr, Y, Matrix)
    end.

add_gears(Acc, N, Stars) ->
    F = fun(XY, {SumAcc, MapAcc}) ->
                {V, C} = maps:get(XY, MapAcc, {1, 0}),
                {NewV, NewC} = {V * N, C + 1},
                NewSum =
                    case C of
                        1 ->
                            SumAcc + NewV;
                        2 ->
                            SumAcc - V;
                        _ ->
                            SumAcc
                    end,
                NewMap = maps:put(XY, {NewV, NewC}, MapAcc),
                {NewSum, NewMap}
        end,
    lists:foldl(F, Acc, Stars).

-define(STAR(X, Y), (array:get(X, array:get(Y, Matrix)) =:= $*)).

stars(Acc, N, X, Y, Matrix) ->
    LeftAcc =
        case N of
            0 when X > 0 ->
                LeftTopAcc =
                    case Y > 0 andalso ?STAR(X - 1, Y - 1) of
                        true ->
                            [{X - 1, Y - 1} | Acc];
                        false ->
                            Acc
                    end,
                LeftMidAcc =
                    case ?STAR(X - 1, Y) of
                        true ->
                            [{X - 1, Y} | LeftTopAcc];
                        false ->
                            LeftTopAcc
                    end,
                LeftBotAcc =
                    case Y < array:size(Matrix) - 1 andalso ?STAR(X-1, Y+1) of
                        true ->
                            [{X - 1, Y + 1} | LeftMidAcc];
                        false ->
                            LeftMidAcc
                    end,
                LeftBotAcc;
            _ ->
                Acc
        end,
    UpAcc =
        case Y > 0 andalso ?STAR(X, Y - 1) of
            true ->
                [{X, Y - 1} | LeftAcc];
            false ->
                LeftAcc
        end,
    SelfAcc =
        case ?STAR(X, Y) of
            true ->
                [{X, Y} | UpAcc];
            false ->
                UpAcc
        end,
    DownAcc =
        case Y < array:size(Matrix) - 1 andalso ?STAR(X, Y + 1) of
            true ->
                [{X, Y + 1} | SelfAcc];
            false ->
                SelfAcc
        end,
    DownAcc.
