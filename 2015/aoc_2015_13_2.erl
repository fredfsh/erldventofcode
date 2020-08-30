-module(aoc_2015_13_2).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl(maps:new(), sets:new()).

input_impl(M, S) ->
    case io:get_line("") of
        eof ->
            {M, S};
        L ->
            {Left, Right, N} =
                case string:split(string:trim(L, trailing, ".\n"), " ", all) of
                    [A, _, "lose", Ns, _, _, _, _, _, _, B] ->
                        {A, B, -list_to_integer(Ns)};
                    [A, _, "gain", Ns, _, _, _, _, _, _, B] ->
                        {A, B, list_to_integer(Ns)}
                end,
            S2 = sets:add_element(Left, S),
            S3 = sets:add_element(Right, S2),
            M2 = maps:put({Left, Right}, N, M),
            input_impl(M2, S3)
    end.

do({M, Unseated}) ->
    do_impl(M, Unseated).

do_impl(M, Unseated) ->
    F = fun(X, Acc) ->
                max(Acc, dfs(X, 0, sets:del_element(X, Unseated), M))
        end,
    lists:foldl(F, 0, sets:to_list(Unseated)).

dfs(Last, HAcc, Unseated, M) ->
    case sets:size(Unseated) of
        0 ->
            HAcc;
        _ ->
            L = sets:to_list(Unseated),
            F = fun(Who, MaxAcc) ->
                        HAcc2 = HAcc +
                            maps:get({Last, Who}, M) +
                            maps:get({Who, Last}, M),
                        Unseated2 = sets:del_element(Who, Unseated),
                        max(MaxAcc, dfs(Who, HAcc2, Unseated2, M))
                end,
            lists:foldl(F, 0, L)
    end.
