-module(aoc_2016_9_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl([], 0, []).

run_impl([], Acc, Stack) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(X, Acc, Stack)
    end;

run_impl([$( | T], Acc, Stack) ->
    {Rest, MarkLen, Len, Times} = mark(T),
    NewStack =
        case Stack of
            [] ->
                [{Len, Times}];
            [{TopLen, TopTimes} | TT] when TopLen =:= Len + MarkLen ->
                [{Len, Times * TopTimes} | TT];
            [{TopLen, TopTimes} | TT] ->
                [{Len, Times * TopTimes},
                 {TopLen - MarkLen - Len, TopTimes} | TT]
        end,
    run_impl(Rest, Acc, NewStack);
run_impl([_ | T], Acc, []) ->
    run_impl(T, Acc + 1, []);
run_impl([_ | T], Acc, [{1, Times} | TT]) ->
    run_impl(T, Acc + Times, TT);
run_impl([_ | T], Acc, [{Len, Times} | TT]) ->
    run_impl(T, Acc + Times, [{Len - 1, Times} | TT]).

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

mark(T) ->
    mark_impl(T, 1, {len, 0}).

mark_impl([], MarkLen, State) ->
    mark_impl(input(), MarkLen, State);
mark_impl([$x | T], MarkLen, {len, Len}) ->
    mark_impl(T, MarkLen + 1, {times, 0, Len});
mark_impl([N | T], MarkLen, {len, Len}) ->
    mark_impl(T, MarkLen + 1, {len, Len * 10 + (N - $0)});

mark_impl([$) | T], MarkLen, {times, Times, Len}) ->
    {T, MarkLen + 1, Len, Times};
mark_impl([N | T], MarkLen, {times, Times, Len}) ->
    mark_impl(T, MarkLen + 1, {times, Times * 10 + (N - $0), Len}).
