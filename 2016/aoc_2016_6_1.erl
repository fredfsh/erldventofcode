-module(aoc_2016_6_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl().

run_impl() ->
    do(input()).

input() ->
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "~s") of
        eof ->
            Acc;
        {ok, [X]} ->
            input_impl([X | Acc])
    end.

do(X) ->
    do_impl(X, "").

do_impl([[] | _], Acc) ->
    lists:reverse(Acc);
do_impl(M, Acc) ->
    Col = [hd(L) || L <- M],
    F = fun(X, Map) ->
                maps:update_with(X, fun(N) -> N + 1 end, 1, Map)
        end,
    Map = lists:foldl(F, maps:new(), Col),
    do_impl([tl(L) || L <- M], [most(Map) | Acc]).

most(Map) ->
    PL = lists:keysort(2, maps:to_list(Map)),
    element(1, lists:last(PL)).
