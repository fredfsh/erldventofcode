-module(aoc_2020_14_2).

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
    input_impl([]).

input_impl(Acc) ->
    case io:get_line("") of
        eof ->
            lists:reverse(Acc);
        L ->
            L2 = string:trim(L, trailing),
            [Left, Right] = string:split(L2, "="),
            case Left of
                "mask " ->
                    input_impl([string:trim(Right, leading) | Acc]);
                _ ->
                    {Addr, _} = string:to_integer(string:slice(Left, 4)),
                    {Val, _} = string:to_integer(string:trim(Right, leading)),
                    input_impl([{Addr, Val} | Acc])
            end
    end.

-define(TAB, map).

do(X) ->
    ets:new(?TAB, [named_table]),
    do_impl(X, undefined).

do_impl([], _) ->
    sum();
do_impl([{Addr, Val} | T], Mask) ->
    puts(Addr, Mask, Val),
    do_impl(T, Mask);
do_impl([Bitmap | T], _Mask) ->
    do_impl(T, Bitmap).

sum() ->
    ets:foldl(fun({_, Val}, Acc) -> Acc + Val end, 0, ?TAB).

puts(Addr, Mask, Val) ->
    M = mask(Addr, Mask),
    puts_impl(M, Val).

mask(Addr, Mask) ->
    mask_impl(Addr, lists:reverse(Mask), []).

mask_impl(0, [], Acc) ->
    Acc;
mask_impl(0, Mask, Acc) ->
    lists:append(lists:reverse(Mask), Acc);
mask_impl(Addr, [], Acc) when Addr band 1 =:= 0 ->
    mask_impl(Addr bsr 1, [], [$0 | Acc]);
mask_impl(Addr, [], Acc) when Addr band 1 =:= 1 ->
    mask_impl(Addr bsr 1, [], [$1 | Acc]);
mask_impl(Addr, [$X | T], Acc) ->
    mask_impl(Addr bsr 1, T, [$X | Acc]);
mask_impl(Addr, [$1 | T], Acc) ->
    mask_impl(Addr bsr 1, T, [$1 | Acc]);
mask_impl(Addr, [$0 | T], Acc) when Addr band 1 =:= 0 ->
    mask_impl(Addr bsr 1, T, [$0 | Acc]);
mask_impl(Addr, [$0 | T], Acc) when Addr band 1 =:= 1 ->
    mask_impl(Addr bsr 1, T, [$1 | Acc]).

puts_impl(Mask, Val) ->
    gen(Val, lists:reverse(Mask), 0, 1).

gen(Val, [], Addr, _) ->
    ets:insert(?TAB, {Addr, Val});
gen(Val, [$0 | T], Addr, Bit) ->
    gen(Val, T, Addr, Bit bsl 1);
gen(Val, [$1 | T], Addr, Bit) ->
    gen(Val, T, Addr + Bit, Bit bsl 1);
gen(Val, [$X | T], Addr, Bit) ->
    gen(Val, T, Addr, Bit bsl 1),
    gen(Val, T, Addr + Bit, Bit bsl 1).
