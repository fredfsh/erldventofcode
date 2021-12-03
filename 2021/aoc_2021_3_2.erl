-module(aoc_2021_3_2).

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
            L
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(L) ->
    most(L) * least(L).

most(L) ->
    Len = length(hd(L)),
    L1 = [list_to_integer(X, 2) || X <- L],
    Mask = 1 bsl (Len - 1),
    filter_most(sets:from_list(L1), Mask).

filter_most(Set, Mask) ->
    case sets:size(Set) of
        1 ->
            hd(sets:to_list(Set));
        _ ->
            F = fun(X, Acc) when X band Mask =/= 0 ->
                        Acc + 1;
                   (_, Acc) ->
                        Acc
                end,
            Count = sets:fold(F, 0, Set),
            Keep = case Count >= sets:size(Set) - Count of
                       true ->
                           Mask;
                       false ->
                           0
                   end,
            filter_most(sets:filter(fun(X) -> X band Mask =:= Keep end, Set),
                        Mask bsr 1)
    end.

least(L) ->
    Len = length(hd(L)),
    L1 = [list_to_integer(X, 2) || X <- L],
    Mask = 1 bsl (Len - 1),
    filter_least(sets:from_list(L1), Mask).

filter_least(Set, Mask) ->
    case sets:size(Set) of
        1 ->
            hd(sets:to_list(Set));
        _ ->
            F = fun(X, Acc) when X band Mask =:= 0 ->
                        Acc + 1;
                   (_, Acc) ->
                        Acc
                end,
            Count = sets:fold(F, 0, Set),
            Keep = case Count =< sets:size(Set) - Count of
                       true ->
                           0;
                       false ->
                           Mask
                   end,
            filter_least(sets:filter(fun(X) -> X band Mask =:= Keep end, Set),
                        Mask bsr 1)
    end.
