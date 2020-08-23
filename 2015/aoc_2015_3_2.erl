-module(aoc_2015_3_2).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

%% len(In) < 10000
do(In) ->
    Set = sets:new(),
    do_impl(In, 1, 0, 0, 0, 0, sets:add_element({0, 0}, Set)).

do_impl([], N, _X, _Y, _A, _B, _Set) ->
    N;
do_impl([C], N, X, Y, _A, _B, Set) ->
    {X2, Y2} = move(C, X, Y),
    {N2, _} = update(N, X2, Y2, Set),
    N2;
do_impl([C1, C2 | T], N, X, Y, A, B, Set) ->
    {X2, Y2} = move(C1, X, Y),
    {N2, Set2} = update(N, X2, Y2, Set),
    {A2, B2} = move(C2, A, B),
    {N3, Set3} = update(N2, A2, B2, Set2),
    do_impl(T, N3, X2, Y2, A2, B2, Set3).

update(N, X, Y, Set) ->
    case sets:is_element({X, Y}, Set) of
        true ->
            {N, Set};
        _ ->
            {N + 1, sets:add_element({X, Y}, Set)}
    end.

move($^, X, Y) -> {X, Y + 1};
move($v, X, Y) -> {X, Y - 1};
move($>, X, Y) -> {X + 1, Y};
move($<, X, Y) -> {X - 1, Y}.
