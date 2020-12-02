-module(aoc_2016_2_2).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~.16B~n", [Out]),
    ok.

do() ->
    do_impl(0, 5).

do_impl(Acc, Last) ->
    case io:fread("", "~s") of
        eof ->
            Acc;
        {ok, [In]} ->
            Next = digit(Last, In),
            do_impl(Acc * 16 + Next, Next)
    end.

digit(Last, []) ->
    Last;
digit(Last, [D | T]) ->
    digit(move(Last, D), T).

move(X, $U) when X =:= 3 -> 1;
move(X, $U) when X >= 6, X =< 8 -> X - 4;
move(X, $U) when X >= 16#A, X =< 16#C -> X - 4;
move(X, $U) when X =:= 16#D -> 16#B;
move(X, $U) -> X;
move(X, $D) when X =:= 1 -> 3;
move(X, $D) when X >= 2, X =< 4 -> X + 4;
move(X, $D) when X >= 6, X =< 8 -> X + 4;
move(X, $D) when X =:= 16#B -> 16#D;
move(X, $D) -> X;
move(X, $L) when X =:= 6 -> 5;
move(X, $L) when X rem 4 =:= 3; X rem 4 =:= 0 -> X - 1;
move(X, $L) when X =:= 9 -> 8;
move(X, $L) -> X;
move(X, $R) when X =:= 5 -> 6;
move(X, $R) when X rem 4 =:= 2; X rem 4 =:= 3 -> X + 1;
move(X, $R) when X =:= 8 -> 9;
move(X, $R) -> X.
