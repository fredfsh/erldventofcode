-module(aoc_2020_2_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(Acc) ->
    case io:fread("", "~d-~d ~c: ~s") of
        eof ->
            Acc;
        {ok, [Low, High, [Char], Str]} ->
            case valid(Low, High, Char, Str) of
                true ->
                    do_impl(Acc + 1);
                _ ->
                    do_impl(Acc)
            end
    end.

valid(L, H, C, S) ->
    valid_impl(L, H, C, S, 0).

valid_impl(L, H, _C, [], X) ->
    X >= L andalso X =< H;
valid_impl(L, H, C, [C | T], X) ->
    valid_impl(L, H, C, T, X + 1);
valid_impl(L, H, C, [_ | T], X) ->
    valid_impl(L, H, C, T, X).
