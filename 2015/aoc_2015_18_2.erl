-module(aoc_2015_18_2).

-export([start/0]).

start() ->
    input(),
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

-define(TAB, table).
-define(D, 100).
-define(N, 100).

input() ->
    ets:new(?TAB, [named_table]),
    input_impl(1),
    ets:insert(?TAB, [{{X, 0}, 0} || X <- lists:seq(0, ?D + 1)]),
    ets:insert(?TAB, [{{0, Y}, 0} || Y <- lists:seq(1, ?D)]),
    ets:insert(?TAB, [{{?D + 1, Y}, 0} || Y <- lists:seq(1, ?D)]),
    ets:insert(?TAB, [{{X, ?D + 1}, 0} || X <- lists:seq(0, ?D + 1)]),
    ets:insert(?TAB, [{{1, 1}, 1}, {{?D, 1}, 1}, {{1, ?D}, 1}, {{?D, ?D}, 1}]).

input_impl(Y) ->
    case io:fread("", "~s") of
        eof ->
            ok;
        {ok, [L]} ->
            init(1, Y, L),
            input_impl(Y + 1)
    end.

init(_X, _Y, []) ->
    ok;
init(X, Y, [$. | T]) ->
    ets:insert(?TAB, {{X, Y}, 0}),
    init(X + 1, Y, T);
init(X, Y, [$# | T]) ->
    ets:insert(?TAB, {{X, Y}, 1}),
    init(X + 1, Y, T).

do() ->
    do_impl(1).

do_impl(I) when I > ?N ->
    ets:select_count(?TAB, [{{'$1', '$2'},
                             [{'=:=', {'band', '$2', 1}, 1}],
                             [true]}]);
do_impl(I) ->
    Updates = evolve(2, 1, []),
    ets:insert(?TAB, Updates),
    do_impl(I + 1).

-define(DS, [{-1, -1}, {0, -1}, {1, -1},
             {-1, 0},           {1, 0},
             {-1, 1},  {0, 1},  {1, 1}]).
evolve(?D, ?D, Acc) ->
    Acc;
evolve(?D, 1, Acc) ->
    evolve(1, 2, Acc);
evolve(1, ?D, Acc) ->
    evolve(2, ?D, Acc);
evolve(X, Y, Acc) ->
    Sum = lists:sum([light(X + DX, Y + DY) || {DX, DY} <- ?DS]),
    NewAcc =
        case {light(X, Y), Sum} of
            {1, S} when S < 2; S > 3 ->
                [{{X, Y}, 0} | Acc];
            {0, 3} ->
                [{{X, Y}, 1} | Acc];
            _ ->
                Acc
        end,
    {NewX, NewY} =
        case X of
            ?D ->
                {1, Y + 1};
            _ ->
                {X + 1, Y}
        end,
    evolve(NewX, NewY, NewAcc).

light(X, Y) ->
    ets:lookup_element(?TAB, {X, Y}, 2).
