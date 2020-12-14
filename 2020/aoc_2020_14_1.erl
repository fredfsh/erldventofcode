-module(aoc_2020_14_1).

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

do(X) ->
    do_impl(X, undefined, maps:new()).

do_impl([], _, Map) ->
    sum(Map);
do_impl([{Addr, Val} | T], Mask, Map) ->
    NewVal = mask(Mask, Val),
    do_impl(T, Mask, map_put(Map, Addr, NewVal));
do_impl([Bitmap | T], _Mask, Map) ->
    do_impl(T, Bitmap, Map).

sum(Map) ->
    maps:fold(fun(_, Val, Acc) -> Acc + Val end, 0, Map).

mask(Mask, Val) ->
    mask_impl(lists:reverse(Mask), 1, Val).

mask_impl([], _, Val) ->
    Val;
mask_impl([$X | T], Bit, Val) ->
    mask_impl(T, Bit bsl 1, Val);
mask_impl([$1 | T], Bit, Val) ->
    mask_impl(T, Bit bsl 1, Val bor Bit);
mask_impl([$0 | T], Bit, Val) when Val band Bit =:= 0 ->
    mask_impl(T, Bit bsl 1, Val);
mask_impl([$0 | T], Bit, Val) ->
    mask_impl(T, Bit bsl 1, Val - Bit).


map_put(Map, Addr, NewVal) ->
    maps:put(Addr, NewVal, Map).
