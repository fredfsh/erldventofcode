-module(aoc_2018_11_2).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    [].

do(X) ->
    ets:new(memo, [named_table]),
    ets:new(row, [named_table]),
    ets:new(col, [named_table]),
    Arr = fill(X),
    largest(Arr).

-define(N, 300).

fill(SN) ->
    F = fun(Y, Acc) ->
                G = fun(X, Acc2) ->
                            Rack = X + 10,
                            Power = (Rack * Y + SN) * Rack rem 1000 div 100 - 5,
                            array:set(X, Power, Acc2)
                    end,
                Arr = lists:foldl(G, array:new(?N + 1), lists:seq(1, ?N)),
                array:set(Y, Arr, Acc)
        end,
    lists:foldl(F, array:new(?N + 1), lists:seq(1, ?N)).

largest(Arr) ->
    F = fun(Y, FAcc) ->
                G = fun(X, GAcc) ->
                            MaxSize = min(?N - X + 1, ?N - Y + 1),
                            H = fun(Size, {_, _, _, Max} = HAcc) ->
                                        case dp(X, Y, Size, Arr) of
                                            N when N > Max ->
                                                {X, Y, Size, N};
                                            _ ->
                                                HAcc
                                        end
                                end,
                            lists:foldl(H, GAcc, lists:seq(1, MaxSize))
                    end,
                lists:foldl(G, FAcc, lists:seq(1, ?N))
        end,
    Init = {1, 1, 1, val(1, 1, Arr)},
    {X, Y, Size, _} = lists:foldl(F, Init, lists:seq(1, ?N)),
    {X, Y, Size}.

val(X, Y, Arr) ->
    array:get(X, array:get(Y, Arr)).

dp(X, Y, 1, Arr) ->
    val(X, Y, Arr);
dp(X, Y, Size, Arr) ->
    case ets:lookup(memo, {X, Y, Size}) of
        [{_, V}] ->
            V;
        [] ->
            Square = dp(X + 1, Y + 1, Size - 1, Arr),
            Row = row(Y, X, Size, Arr),
            Col = col(X, Y, Size, Arr),
            Res = Square + Row + Col - val(X, Y, Arr),
            ets:insert_new(memo, {{X, Y, Size}, Res}),
            Res
    end.

row(Y, X, 1, Arr) ->
    val(X, Y, Arr);
row(Y, X, Size, Arr) ->
    case ets:lookup(row, {Y, X, Size}) of
        [{_, V}] ->
            V;
        [] ->
            Res = row(Y, X + 1, Size - 1, Arr) + val(X, Y, Arr),
            ets:insert_new(row, {{Y, X, Size}, Res}),
            Res
    end.

col(X, Y, 1, Arr) ->
    val(X, Y, Arr);
col(X, Y, Size, Arr) ->
    case ets:lookup(col, {X, Y, Size}) of
        [{_, V}] ->
            V;
        [] ->
            Res = col(X, Y + 1, Size - 1, Arr) + val(X, Y, Arr),
            ets:insert_new(col, {{X, Y, Size}, Res}),
            Res
    end.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    X.
