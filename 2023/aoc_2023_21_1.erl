-module(aoc_2023_21_1).

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
        {ok, [L]} ->
            array:from_list(L)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(STEPS, 64).

fin(Matrix) ->
    SXY = start(Matrix),
    garden_plots(?STEPS, sets:from_list([SXY]), Matrix).

start(Matrix) ->
    F = fun(Y, Arr, {undefined, _}) ->
                G = fun(X, $S, undefined) ->
                            X;
                       (_, _, Acc) ->
                            Acc
                    end,
                {array:foldl(G, undefined, Arr), Y};
           (_, _, Acc) ->
                Acc
        end,
    array:foldl(F, {undefined, undefined}, Matrix).

-define(Ds, [{1, 0}, {0, -1}, {-1, 0}, {0, 1}]).
-define(WALL(X, Y), (array:get(X, array:get(Y, Matrix)) =:= $#)).

garden_plots(0, Acc, _) ->
    sets:size(Acc);
garden_plots(Steps, Acc, Matrix) ->
    Rows = array:size(Matrix),
    Cols = array:size(array:get(0, Matrix)),
    F = fun({X, Y}, FAcc) ->
                G = fun({DX, DY}, GAcc) ->
                            case {X + DX, Y + DY} of
                                {NX, _} when NX < 0; NX >= Cols ->
                                    GAcc;
                                {_, NY} when NY < 0; NY >= Rows ->
                                    GAcc;
                                {NX, NY} = NXY ->
                                    case ?WALL(NX, NY) of
                                        true ->
                                            GAcc;
                                        false ->
                                            sets:add_element(NXY, GAcc)
                                    end
                            end
                    end,
                lists:foldl(G, FAcc, ?Ds)
        end,
    garden_plots(Steps - 1, sets:fold(F, sets:new(), Acc), Matrix).
