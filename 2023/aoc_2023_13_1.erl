-module(aoc_2023_13_1).

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
            Matrix = array:set(0, array:from_list(string:trim(L)), array:new()),
            input_rest(Matrix)
    end.

input_rest(Matrix) ->
    case io:get_line("") of
        eof ->
            Matrix;
        "\n" ->
            Matrix;
        L ->
            Arr = array:from_list(string:trim(L)),
            NewMatrix = array:set(array:size(Matrix), Arr, Matrix),
            input_rest(NewMatrix)
    end.

ini() ->
    0.

do(X) ->
    case vertical(X) of
        undefined ->
            100 * horizontal(X);
        Columns ->
            Columns
    end.

vertical(Matrix) ->
    Columns = array:size(array:get(0, Matrix)),
    Possible = lists:seq(1, Columns - 1),
    F = fun(_, Arr, FAcc) ->
                G = fun(Cols) ->
                            vsym(Cols - 1, Cols, Arr)
                    end,
                lists:filter(G, FAcc)
        end,
    case array:foldl(F, Possible, Matrix) of
        [] ->
            undefined;
        [Res] ->
            Res
    end.

-define(a(I), array:get(I, Arr)).

vsym(I, J, Arr) ->
    case I >= 0 andalso J < array:size(Arr) of
        true ->
            case ?a(I) =:= ?a(J) of
                true ->
                    vsym(I - 1, J + 1, Arr);
                false ->
                    false
            end;
        false ->
            true
    end.

horizontal(Matrix) ->
    Rows = array:size(Matrix),
    Possible = lists:seq(1, Rows - 1),
    Cols = array:size(array:get(0, Matrix)),
    F = fun(X, FAcc) ->
                G = fun(R) ->
                            hsym(R - 1, R, X, Matrix)
                    end,
                lists:filter(G, FAcc)
        end,
    case lists:foldl(F, Possible, lists:seq(0, Cols - 1)) of
        [] ->
            undefined;
        [Res] ->
            Res
    end.

-define(b(I), array:get(X, array:get(I, Matrix))).

hsym(I, J, X, Matrix) ->
    case I >= 0 andalso J < array:size(Matrix) of
        true ->
            case ?b(I) =:= ?b(J) of
                true ->
                    hsym(I - 1, J + 1, X, Matrix);
                false ->
                    false
            end;
        false ->
            true
    end.


acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
