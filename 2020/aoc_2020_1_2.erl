-module(aoc_2020_1_2).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    %% may not work if there are duplicates in input
    do_impl(sets:new()).

do_impl(Set) ->
    {ok, [N]} = io:fread("", "~d"),
    case product(2020 - N, Set) of
        X when is_integer(X) ->
            X * N;
        _ ->
            do_impl(sets:add_element(N, Set))
    end.

product(Sum, Set) ->
    F = fun(X, undefined) ->
                Rem = sets:del_element(X, Set),
                case sets:is_element(Sum - X, Rem) of
                    true ->
                        (Sum - X) * X;
                    _ ->
                        undefined
                end;
           (_, Product) ->
                Product
        end,
    sets:fold(F, undefined, Set).
