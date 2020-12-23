-module(aoc_2020_23_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

-define(N, 1000000).
-define(INPUT, [7, 8, 4, 2, 3, 5, 9, 1, 6]).
-define(MOVES, 10000000).
-define(TAB, next).

run() ->
    init(),
    do().

init() ->
    ets:new(?TAB, [named_table]),
    init_impl(?INPUT).

init_impl([X, Y | T]) ->
    ets:insert(?TAB, {X, Y}),
    init_impl([Y | T]);
init_impl([X]) ->
    I = length(?INPUT) + 1,
    ets:insert(?TAB, {X, I}),
    init_impl(I);
init_impl(?N) ->
    [First | _] = ?INPUT,
    ets:insert(?TAB, {?N, First});
init_impl(I) ->
    ets:insert(?TAB, {I, I + 1}),
    init_impl(I + 1).

do() ->
    [First | _] = ?INPUT,
    do_impl(First, 1).

do_impl(_Cur, Moves) when Moves > ?MOVES ->
    Cup1 = next(1),
    Cup2 = next(Cup1),
    Cup1 * Cup2;
do_impl(Cur, Moves) ->
    Dest = dest(Cur),
    move_three(Cur, Dest),
    do_impl(next(Cur), Moves + 1).

next(I) ->
    ets:lookup_element(?TAB, I, 2).

dest(Cur) ->
    {First, Mid, Last} = three(Cur),
    dest_impl(Cur - 1, First, Mid, Last).

dest_impl(Dest, First, Mid, Last) when Dest < 1 ->
    dest_impl(?N, First, Mid, Last);
dest_impl(Dest, Dest, Mid, Last) ->
    dest_impl(Dest - 1, Dest, Mid, Last);
dest_impl(Dest, First, Dest, Last) ->
    dest_impl(Dest - 1, First, Dest, Last);
dest_impl(Dest, First, Mid, Dest) ->
    dest_impl(Dest - 1, First, Mid, Dest);
dest_impl(Dest, _First, _Mid, _Last) ->
    Dest.

three(Cur) ->
    First = next(Cur),
    Mid = next(First),
    Last = next(Mid),
    {First, Mid, Last}.

move_three(Cur, Dest) ->
    {First, _Mid, Last} = three(Cur),
    LastNext = next(Last),
    DestNext = next(Dest),
    update_next(Last, DestNext),
    update_next(Dest, First),
    update_next(Cur, LastNext).

update_next(Who, Next) ->
    ets:update_element(?TAB, Who, {2, Next}).
