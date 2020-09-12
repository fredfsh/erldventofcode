-module(aoc_2015_23_2).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl(0, maps:new()).

input_impl(N, M) ->
    case io:fread("", "~a") of
        eof ->
            {N, M};
        {ok, [Arith]} when Arith =:= hlf; Arith =:= tpl; Arith =:= inc ->
            {ok, [R]} = io:fread("", "~2a"),
            input_impl(N + 1, maps:put(N, {Arith, R}, M));
        {ok, [jmp]} ->
            {ok, [Offset]} = io:fread("", "~d"),
            input_impl(N + 1, maps:put(N, {jmp, Offset}, M));
        {ok, [Jmp]} when Jmp =:= jie; Jmp =:= jio ->
            {ok, [R, Offset]} = io:fread("", "~2a, ~d"),
            input_impl(N + 1, maps:put(N, {Jmp, R, Offset}, M))
    end.

do({N, M}) ->
    do_impl(1, 0, 0, N, M).

do_impl(_A, B, Addr, N, _M) when Addr < 0; Addr >= N ->
    B;
do_impl(A, B, Addr, N, M) ->
    case maps:get(Addr, M) of
        {jmp, Offset} ->
            do_impl(A, B, Addr + Offset, N, M);
        {jie, a, Offset} when A band 1 =:= 0 ->
            do_impl(A, B, Addr + Offset, N, M);
        {jie, b, Offset} when B band 1 =:= 0 ->
            do_impl(A, B, Addr + Offset, N, M);
        {jie, _, _} ->
            do_impl(A, B, Addr + 1, N, M);
        {jio, a, Offset} when A =:= 1 ->
            do_impl(A, B, Addr + Offset, N, M);
        {jio, b, Offset} when B =:= 1 ->
            do_impl(A, B, Addr + Offset, N, M);
        {jio, _, _} ->
            do_impl(A, B, Addr + 1, N, M);
        {hlf, a} ->
            do_impl(A bsr 1, B, Addr + 1, N, M);
        {hlf, b} ->
            do_impl(A, B bsr 1, Addr + 1, N, M);
        {tpl, a} ->
            do_impl((A bsl 1) + A, B, Addr + 1, N, M);
        {tpl, b} ->
            do_impl(A, (B bsl 1) + B, Addr + 1, N, M);
        {inc, a} ->
            do_impl(A + 1, B, Addr + 1, N, M);
        {inc, b} ->
            do_impl(A, B + 1, Addr + 1, N, M)
    end.
