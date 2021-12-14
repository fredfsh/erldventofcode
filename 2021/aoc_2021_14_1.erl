-module(aoc_2021_14_1).

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

-define(STEPS, 10).

fin({Init, Mapping}) ->
    react(0, Init, Mapping).

react(?STEPS, L, _) ->
    F = fun(X, Acc) ->
                maps:update_with(X, fun(C) -> C + 1 end, 1, Acc)
        end,
    Counts = maps:to_list(lists:foldl(F, maps:new(), L)),
    Sorted = lists:keysort(2, Counts),
    {_, Min} = hd(Sorted),
    {_, Max} = lists:last(Sorted),
    Max - Min;
react(Steps, [H | T], Mapping) ->
    react(Steps + 1, react_impl([H], H, T, Mapping), Mapping).

react_impl(Acc, _, [], _) ->
    lists:reverse(Acc);
react_impl(Acc, Last, [H | T], Mapping) ->
    M = maps:get({Last, H}, Mapping),
    react_impl([H, M | Acc], H, T, Mapping).
