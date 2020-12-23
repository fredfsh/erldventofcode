-module(aoc_2020_23_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

-define(N, 9).
-define(INPUT, [7, 8, 4, 2, 3, 5, 9, 1, 6]).
-define(MOVES, 100).

run() ->
    do().

do() ->
    do_impl(?INPUT, 1, 1).

do_impl(L, I, Moves) when I > length(L) ->
%%    io:format(" ---~n L: ~p~n I: ~p~n Moves: ~p~n~n", [L, I, Moves]),
    do_impl(L, I - length(L), Moves);
do_impl(L, _I, Moves) when Moves > ?MOVES ->
    L2 = lists:delete(1, L),
    string:join([[X + $0] || X <- L2], "");
do_impl(L, I, Moves) ->
    {L2, L3, J} = remove3(L, I),
    DestI = dest(L2, J),
    {NewI, L4} = insert(L2, J, L3, DestI),
%%    io:format(" ###~n L: ~p~n I: ~p~n Moves: ~p~n~n", [L4, NewI, Moves + 1]),
    do_impl(L4, NewI, Moves + 1).

remove3(L, I) ->
    remove3_impl(L, I, [], I + 1).

remove3_impl(L, I, Three, _) when length(Three) =:= 3 ->
    {L, lists:reverse(Three), I};
remove3_impl(L, I, Three, J) when J > length(L) ->
    remove3_impl(L, I, Three, 1);
remove3_impl(L, I, Three, J) ->
    PickedUp = lists:nth(J, L),
    remove3_impl(lists:delete(PickedUp, L),
                 case J < I of true -> I - 1; _ -> I end,
                 [PickedUp | Three],
                 J).

dest(L, I) ->
    Current = lists:nth(I, L),
    dest_impl(L, I, Current - 1).

dest_impl(L, I, Dest) ->
    case index(Dest, L) of
        undefined when Dest =< 1 ->
            dest_impl(L, I, ?N);
        undefined ->
            dest_impl(L, I, Dest - 1);
        J ->
            J
    end.

index(Dest, L) ->
    F = fun(X, {I, _Acc}) when X =:= Dest ->
                {I + 1, I};
           (_, {I, Acc}) ->
                {I + 1, Acc}
        end,
    {_, Res} = lists:foldl(F, {1, undefined}, L),
    Res.

insert(L, I, Three, J) ->
    {LL, LR} = lists:split(J, L),
    NL = lists:append([LL, Three, LR]),
    NewI = case J < I of
               true ->
                   I + 3 + 1;
               _ ->
                   I + 1
           end,
    {NewI, NL}.
