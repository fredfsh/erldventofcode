-module(aoc_2015_6_2).

-export([start/0]).

-define(TAB, lights).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    ets:new(?TAB, [named_table]),
    do_impl().

do_impl() ->
    case input() of
        eof ->
            count();
        {Fun, X1, Y1, X2, Y2} ->
            loop(X1, Y1, X2, Y2, Fun),
            do_impl()
    end.

input() ->
    Fun =
        case io:fread("", "~s") of
            eof ->
                eof;
            {ok, ["toggle"]} ->
                fun toggle/1;
            {ok, ["turn"]} ->
                case io:fread("", "~s") of
                    {ok, ["on"]} ->
                        fun turn_on/1;
                    {ok, ["off"]} ->
                        fun turn_off/1
                end
        end,
    case Fun of
        eof ->
            eof;
        _ ->
            {ok, [X1, Y1, X2, Y2]} = io:fread("", "~d,~d through ~d,~d"),
            {Fun, X1, Y1, X2, Y2}
    end.

toggle(X) ->
    ets:update_counter(?TAB, X, 2, {X, 0}).

turn_on(X) ->
    ets:update_counter(?TAB, X, 1, {X, 0}).

turn_off(X) ->
    ets:update_counter(?TAB, X, {2, -1, 0, 0}, {X, 0}).

count() ->
    ets:foldl(fun({_, X}, Acc) -> Acc + X end, 0, ?TAB).

loop(X1, Y1, X2, Y2, Fun) ->
    [Fun(key(X, Y)) || X <- lists:seq(X1, X2), Y <- lists:seq(Y1, Y2)].

key(X, Y) ->
    X * 1000 + Y.
