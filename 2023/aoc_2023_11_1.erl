-module(aoc_2023_11_1).

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
            array:from_list(string:trim(L))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Matrix, Arr) ->
    array:set(array:size(Matrix), Arr, Matrix).

fin(Matrix) ->
    Shifts = shifts(Matrix),
    Gs = galaxies(Matrix),
    lists:sum([dist(G1, G2, Shifts) || G1 <- Gs, G2 <- Gs, G1 =/= G2]) div 2.

-define(a(X, Y), array:get(X, array:get(Y, Matrix))).

shifts(Matrix) ->
    Width = array:size(array:get(0, Matrix)),
    Height = array:size(Matrix),

    FX = fun(I, Acc) ->
                 Last = array:get(array:size(Acc) - 1, Acc),
                 X = I - 1,
                 G = fun(Y) ->
                             ?a(X, Y) =:= $.
                     end,
                 Value = case lists:all(G, lists:seq(0, Height - 1)) of
                             true ->
                                 Last + 9;
                             false ->
                                 Last
                         end,
                 array:set(array:size(Acc), Value, Acc)

         end,
    XShifts = lists:foldl(FX, array:from_list([0]), lists:seq(1, Width)),

    FY = fun(I, Acc) ->
                 Last = array:get(array:size(Acc) - 1, Acc),
                 Y = I - 1,
                 G = fun(X) ->
                             ?a(X, Y) =:= $.
                     end,
                 Value = case lists:all(G, lists:seq(0, Width - 1)) of
                             true ->
                                 Last + 1;
                             false ->
                                 Last
                         end,
                 array:set(array:size(Acc), Value, Acc)

         end,
    YShifts = lists:foldl(FY, array:from_list([0]), lists:seq(1, Height)),

    {XShifts, YShifts}.

galaxies(Matrix) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $#, GAcc) ->
                            [{X, Y} | GAcc];
                       (_, $., GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, [], Matrix).

dist({X1, Y1}, {X2, Y2}, {XShifts, YShifts}) ->
    RX1 = X1 + array:get(X1, XShifts),
    RY1 = Y1 + array:get(Y1, YShifts),
    RX2 = X2 + array:get(X2, XShifts),
    RY2 = Y2 + array:get(Y2, YShifts),
    abs(RX1 - RX2) + abs(RY1 - RY2).
