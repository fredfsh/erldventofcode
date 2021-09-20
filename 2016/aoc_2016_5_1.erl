-module(aoc_2016_5_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Prefix = input(),
    do(Prefix).

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    do_impl("", 1, X).

do_impl(Acc, _, _X) when length(Acc) =:= 8 ->
    Acc;
do_impl(Acc, N, X) ->
    case crypto:hash(md5, X ++ integer_to_list(N)) of
        <<0, 0, 0:4, H:4, _:104>> ->
            do_impl(Acc ++ [hex(H)], N + 1, X);
        _ ->
            do_impl(Acc, N + 1, X)
    end.

hex(N) when N =< 9 ->
    N + $0;
hex(N) ->
    N - 10 + $a.
