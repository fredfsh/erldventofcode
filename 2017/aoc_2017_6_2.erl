-module(aoc_2017_6_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(array:new()).

input_impl(Arr) ->
    case io:fread("", "~d") of
        eof ->
            Arr;
        {ok, [X]} ->
            input_impl(array:set(array:size(Arr), X, Arr))
    end.

do(X) ->
    do_impl(1, X, maps:new()).

do_impl(I, Arr, Seen) ->
    case maps:get(Arr, Seen, undefined) of
        undefined ->
            do_impl(I + 1, redist(Arr), maps:put(Arr, I, Seen));
        N ->
            I - N
    end.

redist(Arr) ->
    F = fun(I, Blocks, {_, MaxBlocks}) when Blocks > MaxBlocks ->
                {I, Blocks};
           (_, _, Acc) ->
                Acc
        end,
    {I, Blocks} = array:foldl(F, {-1, -1}, Arr),
    Arr2 = array:set(I, 0, Arr),
    redist_impl(Arr2, Blocks, I).

redist_impl(Arr, 0, _) ->
    Arr;
redist_impl(Arr, Blocks, I) ->
    J = (I + 1) rem array:size(Arr),
    redist_impl(array:set(J, array:get(J, Arr) + 1, Arr), Blocks - 1, J).
