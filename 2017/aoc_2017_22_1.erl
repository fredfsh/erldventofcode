-module(aoc_2017_22_1).

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
            X
    end.

ini() ->
    {sets:new(), 0, undefined}.

do(X) ->
    X.

acc({Set, Y, _}, L) ->
    F = fun($#, {Acc, X}) ->
                {sets:add_element({X, Y}, Acc), X + 1};
           ($., {Acc, X}) ->
                {Acc, X + 1}
        end,
    {NewSet, _} = lists:foldl(F, {Set, 0}, L),
    {NewSet, Y + 1, length(L)}.

fin({Set, _, N}) ->
    burst(0, 0, (N - 1) div 2, (N - 1) div 2, 0, -1, Set).

-define(N, 10000).

burst(Acc, ?N, _, _, _, _, _) ->
    Acc;
burst(Acc, I, X, Y, DX, DY, Set) ->
    {{NDX, NDY}, NewSet, NewAcc} =
        case sets:is_element({X, Y}, Set) of
            true ->
                {turn_right(DX, DY), sets:del_element({X, Y}, Set), Acc};
            false ->
                {turn_left(DX, DY), sets:add_element({X, Y}, Set), Acc + 1}
        end,
    burst(NewAcc, I + 1, X + NDX, Y + NDY, NDX, NDY, NewSet).

turn_right(0, 1) -> {-1, 0};
turn_right(-1, 0) -> {0, -1};
turn_right(0, -1) -> {1, 0};
turn_right(1, 0) -> {0, 1}.

turn_left(0, 1) -> {1, 0};
turn_left(1, 0) -> {0, -1};
turn_left(0, -1) -> {-1, 0};
turn_left(-1, 0) -> {0, 1}.
