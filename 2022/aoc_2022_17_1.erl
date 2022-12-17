-module(aoc_2022_17_1).

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

-define(ROCKS, 2022).
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

do(Jet) ->
    sim(0, array:from_list([?FLOOR]), 0, Jet).

sim(?ROCKS, Arr, _, _) ->
    array:size(Arr) - 1;
sim(Rocks, Arr, I, Jet) ->
    Rock = lists:nth(Rocks rem 5 + 1, ?PATTERNS),
    {NArr, NI} = fall(2, array:size(Arr) + 3, Arr, I, Jet, Rock),
    sim(Rocks + 1, NArr, NI, Jet).

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
            {NArr, NI}
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
%%    io:format("x=~p, y=~p~n~p~n", [X, Y, Arr]),
%%    io:format("~p~n", [array:to_list(array:get(0, Arr))]),
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

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
