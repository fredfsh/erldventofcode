-module(aoc_2016_2_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0, 5).

do_impl(Acc, Last) ->
    case io:fread("", "~s") of
        eof ->
            Acc;
        {ok, [In]} ->
            Next = digit(Last, In),
            do_impl(Acc * 10 + Next, Next)
    end.

digit(Last, []) ->
    Last;
digit(Last, [D | T]) ->
    digit(move(Last, D), T).

move(X, $U) when X =< 3 -> X;
move(X, $U) -> X - 3;
move(X, $D) when X >= 7 -> X;
move(X, $D) -> X + 3;
move(X, $L) when X rem 3 =:= 1 -> X;
move(X, $L) -> X - 1;
move(X, $R) when X rem 3 =:= 0 -> X;
move(X, $R) -> X + 1.
