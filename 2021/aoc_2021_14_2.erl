-module(aoc_2021_14_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(ini()).

run_impl(Acc) ->
    case input() of
        eof ->
            fin(Acc);
        X ->
            run_impl(acc(Acc, do(X)))
    end.

input() ->
    case io:fread("", "~s -> ~s") of
        eof ->
            eof;
        {ok, [[L, R], [M]]} ->
            {L, R, M}
    end.

ini() ->
    {ok, [Init]} = io:fread("", "~s"),
    io:get_line(""),
    {Init, maps:new()}.

do(X) ->
    X.

acc({Init, Mapping}, {L, R, M}) ->
    {Init, maps:put({L, R}, M, Mapping)}.

-define(STEPS, 40).

-define(TAB, memo).

fin({[H | T], Mapping}) ->
    ets:new(?TAB, [named_table]),
    F = fun(X, {CountsAcc, Last}) ->
                Delta = dp(?STEPS, {Last, X}, Mapping),
                {merge(Delta, CountsAcc), X}
        end,
    {Counts, _} = lists:foldl(F, {maps:from_list([{H, 1}]), H}, T),
    Sorted = lists:keysort(2, maps:to_list(Counts)),
    {_, Min} = hd(Sorted),
    {_, Max} = lists:last(Sorted),
    Max - Min.

dp(Steps, Pair, Mapping) ->
    case ets:lookup(?TAB, {Steps, Pair}) of
        [{_, Counts}] ->
            Counts;
        [] ->
            Counts = dp_impl(Steps, Pair, Mapping),
            ets:insert_new(?TAB, {{Steps, Pair}, Counts}),
            Counts
    end.

dp_impl(1, {_, R} = Pair, Mapping) ->
    case maps:get(Pair, Mapping) of
        R ->
            maps:from_list([{R, 2}]);
        M ->
            maps:from_list([{R, 1}, {M, 1}])
    end;
dp_impl(Steps, {L, R} = Pair, Mapping) ->
    M = maps:get(Pair, Mapping),
    Counts1 = dp(Steps - 1, {L, M}, Mapping),
    Counts2 = dp(Steps - 1, {M, R}, Mapping),
    merge(Counts1, Counts2).

merge(M1, M2) ->
    maps:merge_with(fun(_, V1, V2) -> V1 + V2 end, M1, M2).
