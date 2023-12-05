-module(aoc_2023_3_1).

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
                add_parts(Acc, 0, false, 0, Arr, Y, Matrix)
        end,
    array:foldl(F, 0, Matrix).

add_parts(Acc, N, Part, X, Arr, Y, Matrix) ->
    case {array:size(Arr), array:get(X, Arr)} of
        {X, _} when Part ->
            Acc + N;
        {X, _} ->
            Acc;
        {_, D} when D >= $0, D =< $9 ->
            NewPart = part(Part, N, X, Y, Matrix),
            add_parts(Acc, N * 10 + (D - $0), NewPart, X + 1, Arr, Y, Matrix);
        {_, D} ->
            case D =/= $. orelse part(Part, N, X, Y, Matrix) of
                true ->
                    add_parts(Acc + N, 0, false, X + 1, Arr, Y, Matrix);
                false ->
                    add_parts(Acc, 0, false, X + 1, Arr, Y, Matrix)
            end
    end.

-define(SYMBOL(X, Y), ((array:get(X, array:get(Y, Matrix)) < $0 orelse
                        array:get(X, array:get(Y, Matrix)) > $9) andalso
                       array:get(X, array:get(Y, Matrix)) =/= $.)).

part(true, _, _, _, _) ->
    true;
part(false, N, X, Y, Matrix) ->
    Left =
        case N of
            0 when X > 0 ->
                LeftTop = case Y > 0 of
                              true ->
                                  ?SYMBOL(X - 1, Y - 1);
                              false ->
                                  false
                          end,
                LeftMid = ?SYMBOL(X - 1, Y),
                LeftBot = case Y =:= array:size(Matrix) - 1 of
                              true ->
                                  false;
                              false ->
                                  ?SYMBOL(X - 1, Y + 1)
                          end,
                LeftTop orelse LeftMid orelse LeftBot;
            _ ->
                false
        end,
    Up =
        case Y > 0 of
            true ->
                ?SYMBOL(X, Y - 1);
            false ->
                false
        end,
    Down =
        case Y =:= array:size(Matrix) - 1 of
            true ->
                false;
            false ->
                ?SYMBOL(X, Y + 1)
        end,
    Left orelse Up orelse Down.
