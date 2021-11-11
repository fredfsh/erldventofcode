-module(aoc_2018_3_2).

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
    case io:fread("", "#~d @ ~d,~d: ~dx~d") of
        eof ->
            eof;
        {ok, [ID, X, Y, W, H]} ->
            {ID, X, Y, W, H}
    end.

-define(N, 1000).

ini() ->
    {array:new(?N, {default, array:new(?N, {default, 0})}), sets:new()}.

do(X) ->
    X.

acc({Arr, IDs}, {ID, X, Y, W, H}) ->
    Set = sets:add_element(ID, IDs),
    G = fun(I, {ArrAcc, SetAcc}) ->
                case array:get(I, ArrAcc) of
                    0 ->
                        {array:set(I, ID, ArrAcc), SetAcc};
                    Z ->
                        SetAcc2 = sets:del_element(ID, SetAcc),
                        {ArrAcc, sets:del_element(Z, SetAcc2)}
                end
        end,
    F = fun(I, {ArrAcc, SetAcc}) ->
                Array = array:get(I, ArrAcc),
                L = lists:seq(X, X + W - 1),
                {NewArray, NewSet} = lists:foldl(G, {Array, SetAcc}, L),
                {array:set(I, NewArray, ArrAcc), NewSet}
        end,
    lists:foldl(F, {Arr, Set}, lists:seq(Y, Y + H - 1)).

fin({_, X}) ->
    [Res] = sets:to_list(X),
    Res.
