-module(aoc_2022_17_2).

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
            array:from_list(X)
    end.

ini() ->
    0.

-define(PATTERNS, [array:from_list([array:from_list([0, 1, 2, 3])]),
                   array:from_list([array:from_list([1]),
                                    array:from_list([0, 1, 2]),
                                    array:from_list([1])]),
                   array:from_list([array:from_list([0, 1, 2]),
                                    array:from_list([2]),
                                    array:from_list([2])]),
                   array:from_list([array:from_list([0]),
                                    array:from_list([0]),
                                    array:from_list([0]),
                                    array:from_list([0])]),
                   array:from_list([array:from_list([0, 1]),
                                    array:from_list([0, 1])])
                  ]).
-define(WIDTH, 7).
-define(EMPTY, array:new([?WIDTH, {default, $.}])).
-define(FLOOR, array:new([?WIDTH, {default, $#}])).
-define(MEMO, memo).
-define(HEIGHTS, heights).
-define(ROCKS, 1000000000000).

do(Jet) ->
    ets:new(?MEMO, [named_table]),
    ets:new(?HEIGHTS, [named_table]),
    sim(0, array:from_list([?FLOOR]), 0, Jet).

sim(Rocks, Arr, I, Jet) ->
    NRocks = Rocks + 1,
    IR = Rocks rem 5 + 1,
    Rock = lists:nth(IR, ?PATTERNS),
    {NArr, NI, Signature} = fall(2, array:size(Arr) + 3, Arr, I, Jet, Rock),
    Height = array:size(NArr) - 1,
    Key = {IR, I, Signature},
    case ets:lookup(?MEMO, Key) of
        [] ->
            ets:insert_new(?MEMO, {Key, Height, NRocks}),
            ets:insert_new(?HEIGHTS, {NRocks, Height}),
            sim(NRocks, NArr, NI, Jet);
        [{Key, PrevHeight, PrevRocks}] ->
            Cycle = NRocks - PrevRocks,
            Increase = Height - PrevHeight,
            N = ?ROCKS - PrevRocks,
            Index = N rem Cycle + PrevRocks,
            N div Cycle * Increase + ets:lookup_element(?HEIGHTS, Index, 2)
    end.

fall(X, Y, Arr, I, Jet, Rock) ->
    NX = case array:get(I, Jet) of
             $< ->
                 push(X, Y, Arr, Rock, -1);
             $> ->
                 push(X, Y, Arr, Rock, 1)
         end,
    NI = (I + 1) rem array:size(Jet),
    case down(NX, Y, Arr, Rock) of
        ok ->
            fall(NX, Y - 1, Arr, NI, Jet, Rock);
        NArr ->
            {NArr, NI, signature(Y, NArr)}
    end.

push(X, Y, Arr, Rock, DX) ->
    NX = X + DX,
    F = fun(_, _, false) ->
                false;
           (IY, Row, true) ->
                G = fun(_, _, false) ->
                            false;
                       (_, IX, true) when NX + IX < 0 ->
                            false;
                       (_, IX, true) when NX + IX >= ?WIDTH ->
                            false;
                       (_, IX, true) ->
                            not rock(NX + IX, Y + IY, Arr)
                    end,
                array:foldl(G, true, Row)
        end,
    case array:foldl(F, true, Rock) of
        false ->
            X;
        true ->
            NX
    end.

rock(X, Y, Arr) ->
    case Y >= array:size(Arr) of
        true ->
            false;
        false ->
            array:get(X, array:get(Y, Arr)) =:= $#
    end.

down(X, Y, Arr, Rock) ->
    F = fun(_, _, false) ->
                false;
           (IY, Row, true) ->
                G = fun(_, _, false) ->
                            false;
                       (_, IX, true) ->
                            not rock(X + IX, Y - 1 + IY, Arr)
                    end,
                array:foldl(G, true, Row)
        end,
    case array:foldl(F, true, Rock) of
        true ->
            ok;
        false ->
            settle(Rock, X, Y, Arr)
    end.

settle(Rock, X, Y, Arr) ->
    F = fun(IY, Row, Acc) ->
                G = fun(_, IX, GAcc) ->
                            set(X + IX, Y + IY, GAcc)
                    end,
                array:foldl(G, Acc, Row)
        end,
    array:foldl(F, Arr, Rock).

set(X, Y, Arr) ->
    case Y >= array:size(Arr) of
        true ->
            set(X, Y, array:set(array:size(Arr), ?EMPTY, Arr));
        false ->
            Row = array:get(Y, Arr),
            array:set(Y, array:set(X, $#, Row), Arr)
    end.

signature(Y, Arr) ->
    G = fun(_, $#, Acc) ->
                (Acc bsl 1) bor 1;
           (_, $., Acc) ->
                Acc bsl 1
        end,
    F = fun(I, Acc) ->
                Row = array:get(I, Arr),
                array:foldl(G, Acc, Row)
        end,
    lists:foldl(F, 1, lists:seq(Y, array:size(Arr) - 1)).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
