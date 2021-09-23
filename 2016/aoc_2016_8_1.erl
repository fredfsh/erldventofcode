-module(aoc_2016_8_1).

-export([start/0]).

-define(X, 50).
-define(Y, 6).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(init()).

init() ->
    Arr = array:new(?X, {default, $.}),
    array:new(?Y, {default, Arr}).

run_impl(M) ->
    case input() of
        eof ->
            count(M);
        X ->
            run_impl(do(X, M))
    end.

input() ->
    case io:fread("", "~s ~s") of
        eof ->
            eof;
        {ok, ["rect", Rect]} ->
            [XStr, YStr] = string:split(Rect, "x"),
            {X, _} = string:to_integer(XStr),
            {Y, _} = string:to_integer(YStr),
            {rect, X, Y};
        {ok, ["rotate", RowOrCol]} ->
            {ok, [_, A, B]} = io:fread("", " ~c=~d by ~d"),
            {list_to_atom(RowOrCol), A, B}
    end.

count($#) ->
    1;
count($.) ->
    0;
count(Arr) ->
    array:foldl(fun(_, X, Acc) -> Acc + count(X) end, 0, Arr).

do({rect, A, B}, M) ->
    rect(A, B, M);
do({row, A, B}, M) ->
    row(A, B, M);
do({column, A, B}, M) ->
    col(A, B, M).

rect(A, B, M) ->
    F = fun(Y, FAcc) ->
                L = array:get(Y, FAcc),
                G = fun(X, GAcc) ->
                            array:set(X, $#, GAcc)
                    end,
                NewL = lists:foldl(G, L, lists:seq(0, A - 1)),
                array:set(Y, NewL, FAcc)
        end,
    lists:foldl(F, M, lists:seq(0, B - 1)).

row(A, B, M) ->
    L = array:get(A, M),
    List = array:to_list(L),
    Len = length(List),
    {ListL, ListR} = lists:split(Len - B rem Len, List),
    NewL = array:from_list(lists:append(ListR, ListL)),
    array:set(A, NewL, M).

col(A, B, M) ->
    List = [array:get(A, array:get(Y, M)) || Y <- lists:seq(0, ?Y - 1)],
    Len = length(List),
    {ListA, ListB} = lists:split(Len - B rem Len, List),
    NewL = array:from_list(lists:append(ListB, ListA)),
    F = fun(Y, Val, MAcc) ->
                L = array:get(Y, MAcc),
                array:set(Y, array:set(A, Val, L), MAcc)
        end,
    array:foldl(F, M, NewL).
