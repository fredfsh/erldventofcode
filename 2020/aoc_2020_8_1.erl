-module(aoc_2020_8_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        [] ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl([], 1).

input_impl(PL, I) ->
    case io:fread("", "~a ~d") of
        eof ->
            PL;
        {ok, [Cmd, N]} ->
            input_impl([{I, {Cmd, N}} | PL], I + 1)
    end.

do(PL) ->
    do_impl(PL, sets:new(), 1, 0).

do_impl(PL, Set, I, Acc) ->
    case sets:is_element(I, Set) of
        true ->
            Acc;
        _ ->
            NewSet = sets:add_element(I, Set),
            case proplists:get_value(I, PL) of
                {nop, _} ->
                    do_impl(PL, NewSet, I + 1, Acc);
                {acc, N} ->
                    do_impl(PL, NewSet, I + 1, Acc + N);
                {jmp, N} ->
                    do_impl(PL, NewSet, I + N, Acc)
            end
    end.
