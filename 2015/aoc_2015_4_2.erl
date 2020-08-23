-module(aoc_2015_4_2).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    do_impl(In, 1).

do_impl(In, X) ->
    case crypto:hash(md5, In ++ integer_to_list(X)) of
        <<A:24, _B:104>> when A =:= 0->
            X;
        _ ->
            do_impl(In, X + 1)
    end.
