-module(aoc_2020_8_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        {[], _} ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl([], 1).

input_impl(PL, I) ->
    case io:fread("", "~a ~d") of
        eof ->
            {PL, I};
        {ok, [Cmd, N]} ->
            input_impl([{I, {Cmd, N}} | PL], I + 1)
    end.

do({PL, M}) ->
    do_impl(PL, M, 1).

do_impl(PL, M, I) ->
    case change(PL, M, I) of
        loop ->
            do_impl(PL, M, I + 1);
        Acc ->
            Acc
    end.

change(PL, M, X) ->
    change_impl(PL, M, X, sets:new(), 1, 0).

change_impl(_PL, M, _X, _Set, M, Acc) ->
    Acc;
change_impl(PL, M, X, Set, I, Acc) ->
    case sets:is_element(I, Set) of
        true ->
            loop;
        _ ->
            NewSet = sets:add_element(I, Set),
            case proplists:get_value(I, PL) of
                {nop, N} when I =:= X ->
                    change_impl(PL, M, X, NewSet, I + N, Acc);
                {nop, _} ->
                    change_impl(PL, M, X, NewSet, I + 1, Acc);
                {acc, N} ->
                    change_impl(PL, M, X, NewSet, I + 1, Acc + N);
                {jmp, _} when I =:= X ->
                    change_impl(PL, M, X, NewSet, I + 1, Acc);
                {jmp, N} ->
                    change_impl(PL, M, X, NewSet, I + N, Acc)
            end
    end.
