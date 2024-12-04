-module(aoc_2024_4_2).

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
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Arr) ->
    Rows = array:size(Arr),
    Cols = array:size(array:get(0, Arr)),
    lists:sum([search(X, Y, Arr) || Y <- lists:seq(0, Rows - 1),
                                    X <- lists:seq(0, Cols - 1)]).

-define(D, [{-1, -1, -1, 1, 1, -1, 1, 1},
            {1, -1, 1, 1, -1, -1, -1, 1},
            {-1, 1, 1, 1, -1, -1, 1, -1},
            {-1, -1, 1, -1, -1, 1, 1, 1}]).

-define(ia(X, Y, C),
        (X >= 0 andalso X < array:size(array:get(0, Arr))
         andalso Y >= 0 andalso Y < array:size(Arr)
         andalso array:get(X, array:get(Y, Arr)) =:= C)).

search(X, Y, Arr) ->
    F = fun({MX1, MY1, MX2, MY2, SX1, SY1, SX2, SY2}) ->
                ?ia(X, Y, $A)
                    andalso ?ia(X + MX1, Y + MY1, $M)
                    andalso ?ia(X + MX2, Y + MY2, $M)
                    andalso ?ia(X + SX1, Y + SY1, $S)
                    andalso ?ia(X + SX2, Y + SY2, $S)
        end,
    length(lists:filter(F, ?D)).
