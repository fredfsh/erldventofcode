-module(aoc_2015_2_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(X) ->
    case io:fread("", "~dx~dx~d") of
        {ok, [L, W, H]} ->
            S1 = L * W,
            S2 = L * H,
            S3 = W * H,
            Y = min(min(S1, S2), S3),
            do_impl(X + 2 * S1 + 2 * S2 + 2 * S3 + Y);
        eof ->
            X
    end.
