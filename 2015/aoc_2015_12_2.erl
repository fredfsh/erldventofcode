-module(aoc_2015_12_2).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    do_impl(prep(In), 1, 0, 0).

prep(X) ->
    prep_impl(X, [], []).

prep_impl([], Y, _) ->
    lists:reverse(Y);
prep_impl([$" | T], Y, [$d, $e, $r, $"]) ->
    prep_impl(del_forward(T), del_back(Y), []);
prep_impl([$" | T], Y, Val) when Val =/= [] ->
    prep_impl(T, [$" | Y], []);
prep_impl([X | T], Y, Val) when Val =/= [] ->
    prep_impl(T, [X | Y], [X | Val]);
prep_impl([$:, $" | T], Y, []) ->
    prep_impl(T, [$", $: | Y], [$"]);
prep_impl([X | T], Y, []) ->
    prep_impl(T, [X | Y], []).

del_back(X) ->
    del_back_impl(X, 0).

del_back_impl([], _) ->
    [];
del_back_impl([${ | T], 0) ->
    T;
del_back_impl([${ | T], N) ->
    del_back_impl(T, N - 1);
del_back_impl([$} | T], Braces) ->
    del_back_impl(T, Braces + 1);
del_back_impl([_ | T], Braces) ->
    del_back_impl(T, Braces).

del_forward(X) ->
    del_forward_impl(X, 0).

del_forward_impl([], _) ->
    [];
del_forward_impl([$} | T], 0) ->
    T;
del_forward_impl([$} | T], N) ->
    del_forward_impl(T, N - 1);
del_forward_impl([${ | T], Braces) ->
    del_forward_impl(T, Braces + 1);
del_forward_impl([_ | T], Braces) ->
    del_forward_impl(T, Braces).


do_impl([], _Sign, _Acc, Sum) ->
    Sum;
do_impl([$- | T], _Sign, Acc, Sum) ->
    do_impl(T, -1, Acc, Sum);
do_impl([X | T], Sign, Acc, Sum) when $0 =< X, X =< $9 ->
    do_impl(T, Sign, Acc * 10 + (X - $0), Sum);
do_impl([_ | T], Sign, Acc, Sum) ->
    do_impl(T, 1, 0, Sum + Acc * Sign).
