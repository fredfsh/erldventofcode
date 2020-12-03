-module(aoc_2016_3_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(Acc) ->
    case io:fread("", "~d ~d ~d") of
        eof ->
            Acc;
        {ok, [A, B, C]} ->
            case A + B > C andalso A + C > B andalso B + C > A of
                true ->
                    do_impl(Acc + 1);
                _ ->
                    do_impl(Acc)
            end
    end.
