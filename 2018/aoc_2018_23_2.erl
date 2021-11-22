-module(aoc_2018_23_2).

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
    case io:fread("", "pos=<~d,~d,~d>, r=~d") of
        eof ->
            eof;
        {ok, [X, Y, Z, R]} ->
            {X, Y, Z, R}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(Bots) ->
    F = fun({X, Y, Z, R}, {I, Acc}) ->
                Bot = {{{X + Y + Z - R, left, I}, {X + Y + Z + R, right, I}},
                       {{X + Y - Z - R, left, I}, {X + Y - Z + R, right, I}},
                       {{X + Z - Y - R, left, I}, {X + Z - Y + R, right, I}},
                       {{Y + Z - X - R, left, I}, {Y + Z - X + R, right, I}}},
                {I + 1, maps:put(I, Bot, Acc)}
        end,
    {_, Map} = lists:foldl(F, {1, maps:new()}, Bots),
    Whole = sets:from_list(lists:seq(1, length(Bots))),
    {_, NegDist} = lp({0, undefined}, Whole, [], 1, Map),
    -NegDist.

lp(Max, Indices, Ranges, 5, _) ->
    max(Max, {sets:size(Indices), -mindist(Ranges)});
lp({MaxN, _} = Max, Indices, Ranges, I, Bots) ->
    case sets:size(Indices) < MaxN of
        true ->
            Max;
        false ->
            lp_impl(Max, Indices, Ranges, I, Bots)
    end.

%% L1 <= X + Y + Z <= R1
%% L2 <= X + Y - Z <= R2
%% L3 <= X + Z - Y <= R3
%% L4 <= Y + Z - X <= R4
mindist([{L4, R4}, {L3, R3}, {L2, R2}, {L1, R1}]) ->
    max(max(mindist(L1, R1), mindist(L2, R2)),
        max(mindist(L3, R3), mindist(L4, R4))).

mindist(L, _) when L >= 0  ->
    L;
mindist(_, R) when R =< 0 ->
    -R;
mindist(_, _) ->
    0.

lp_impl(Max, Indices, Ranges, I, Bots) ->
    Bounds = bounds(Indices, I, Bots),
    F = fun({Left, left, Index}, {IndicesAcc, LeftsAcc, MaxAcc}) ->
                {sets:add_element(Index, IndicesAcc), [Left | LeftsAcc], MaxAcc};
           ({Right, right, Index}, {IndicesAcc, [H | T], MaxAcc}) ->
                Score = lp(MaxAcc, IndicesAcc, [{H, Right} | Ranges], I+1, Bots),
                {sets:del_element(Index, IndicesAcc), T, max(MaxAcc, Score)}
        end,
    {_, _, Score} = lists:foldl(F, {sets:new(), [], Max}, Bounds),
    Score.

bounds(Indices, I, Bots) ->
    F = fun(Index, Acc) ->
                {L, R} = element(I, maps:get(Index, Bots)),
                [L, R | Acc]
        end,
    lists:sort(sets:fold(F, [], Indices)).
