-module(aoc_2022_15_1).

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
    Fmt = "Sensor at x=~d, y=~d: closest beacon is at x=~d, y=~d",
    case io:fread("", Fmt) of
        eof ->
            eof;
        {ok, [SX, SY, BX, BY]} ->
            {SX, SY, BX, BY}
    end.

ini() ->
    [].

-define(Y, 2000000).

do({SX, SY, BX, BY}) ->
    case abs(SX - BX) + abs(SY - BY) of
        D when D < abs(SY - ?Y) ->
            [];
        D ->
            DX = D - abs(SY - ?Y),
            X1 = SX - DX,
            X2 = SX + DX,
            case BY of
                ?Y when BX =:= X1, BX =:= X2 ->
                    [];
                ?Y when BX =:= X1 ->
                    [{X1 + 1, X2}];
                ?Y when BX =:= X2 ->
                    [{X1, X2 - 1}];
                ?Y when X1 < BX, BX < X2 ->
                    [{X1, BX - 1}, {BX + 1, X2}];
                _ ->
                    [{X1, X2}]
            end
    end.

acc(Acc, X) ->
    Acc ++ X.

fin(Ranges) ->
    Sorted = lists:sort(Ranges),
    count(Sorted).

count([{HL, HR} | T]) ->
    count_impl(0, HL, HR, T).

count_impl(Acc, L, R, []) ->
    Acc + (R - L + 1);
count_impl(Acc, L, R, [{HL, HR} | T]) when HL > R ->
    count_impl(Acc + (R - L + 1), HL, HR, T);
count_impl(Acc, L, R, [{_, HR} | T]) ->
    count_impl(Acc, L, max(R, HR), T).
