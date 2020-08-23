-module(aoc_2015_3_1).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

%% len(In) < 10000
do(In) ->
    Set = sets:new(),
    do_impl(In, 1, 0, 0, sets:add_element({0, 0}, Set)).

do_impl([], N, _X, _Y, _Set) ->
    N;
do_impl([C | T], N, X, Y, Set) ->
    {X2, Y2} = move(C, X, Y),
    case sets:is_element({X2, Y2}, Set) of
        true ->
            do_impl(T, N, X2, Y2, Set);
        _ ->
            do_impl(T, N + 1, X2, Y2, sets:add_element({X2, Y2}, Set))
    end.

move($^, X, Y) -> {X, Y + 1};
move($v, X, Y) -> {X, Y - 1};
move($>, X, Y) -> {X + 1, Y};
move($<, X, Y) -> {X - 1, Y}.
