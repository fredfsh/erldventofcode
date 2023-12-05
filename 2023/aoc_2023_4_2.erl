-module(aoc_2023_4_2).

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
    case io:fread("", "Card ~d: ") of
        eof ->
            eof;
        {ok, [I]} ->
            L = io:get_line(""),
            [Left, Right] = string:split(L, " | "),
            {I, nums(Left), nums(Right)}
    end.

nums(L) ->
    nums_impl([], undefined, L).

nums_impl(Acc, undefined, []) ->
    lists:reverse(Acc);
nums_impl(Acc, Running, []) ->
    lists:reverse([Running | Acc]);
nums_impl(Acc, undefined, [H | T]) when H >= $0, H =< $9 ->
    nums_impl(Acc, H - $0, T);
nums_impl(Acc, Running, [H | T]) when H >= $0, H =< $9 ->
    nums_impl(Acc, Running * 10 + (H - $0), T);
nums_impl(Acc, undefined, [_ | T]) ->
    nums_impl(Acc, undefined, T);
nums_impl(Acc, Running, [_ | T]) ->
    nums_impl([Running | Acc], undefined, T).

ini() ->
    {0, maps:new()}.

do({I, Left, Right}) ->
    Winning = sets:from_list(Left),
    F = fun(X) -> sets:is_element(X, Winning) end,
    {I, length(lists:filter(F, Right))}.

acc({Scratchcards, Copies}, {I, Matches}) ->
    Total = maps:get(I, Copies, 1),
    Map =
        case Matches of
            0 ->
                Copies;
            X ->
                F = fun(J, Acc) ->
                            G = fun(Y) -> Y + Total end,
                            maps:update_with(J, G, 1 + Total, Acc)
                    end,
                lists:foldl(F, Copies, lists:seq(I + 1, I + X))
        end,
    {Scratchcards + Total, Map}.

fin({Scratchcards, _}) ->
    Scratchcards.
