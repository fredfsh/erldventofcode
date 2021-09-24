-module(aoc_2016_9_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl([], 0, {text, 0}).

%% State :: {text, Marked} | {len, Len} | {times, Len, Times}
run_impl([], Acc, State) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(X, Acc, State)
    end;

run_impl([$( | T], Acc, {text, 0}) ->
    run_impl(T, Acc, {len, 0});
run_impl([_ | T], Acc, {text, 0}) ->
    run_impl(T, Acc + 1, {text, 0});
run_impl([_ | T], Acc, {text, Marked}) ->
    run_impl(T, Acc, {text, Marked - 1});

run_impl([$x | T], Acc, {len, Len}) ->
    run_impl(T, Acc, {times, 0, Len});
run_impl([N | T], Acc, {len, Len}) ->
    run_impl(T, Acc, {len, Len * 10 + (N - $0)});

run_impl([$) | T], Acc, {times, Times, Len}) ->
    run_impl(T, Acc + Times * Len, {text, Len});
run_impl([N | T], Acc, {times, Times, Len}) ->
    run_impl(T, Acc, {times, Times * 10 + (N - $0), Len}).

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.
