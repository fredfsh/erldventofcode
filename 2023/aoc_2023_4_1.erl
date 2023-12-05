-module(aoc_2023_4_1).

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
        {ok, [_]} ->
            L = io:get_line(""),
            [Left, Right] = string:split(L, " | "),
            {nums(Left), nums(Right)}
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
    0.

do({Left, Right}) ->
    Winning = sets:from_list(Left),
    F = fun(X) -> sets:is_element(X, Winning) end,
    case length(lists:filter(F, Right)) of
        0 ->
            0;
        N ->
            trunc(math:pow(2, N - 1))
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
