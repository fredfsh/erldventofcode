-module(aoc_2022_15_2).

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

-define(MIN, 0).
-define(MAX, 4000000).

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(L) ->
    fin_impl(?MIN, L).

fin_impl(Y, L) ->
    F = fun({SX, SY, BX, BY}, Acc) ->
                case abs(SX - BX) + abs(SY - BY) of
                    D when D < abs(SY - Y) ->
                        Acc;
                    D ->
                        DX = D - abs(SY - Y),
                        [{SX - DX, SX + DX} | Acc]
                end
        end,
    Ranges = lists:foldl(F, [], L),
    Sorted = lists:sort(Ranges),
    case find(Sorted) of
        undefined ->
            fin_impl(Y + 1, L);
        X ->
            X * ?MAX + Y
    end.

find(L) ->
    find_impl(-2, -1, L).

find_impl(_L, R, []) when R =:= ?MAX - 1 ->
    ?MAX;
find_impl(_L, _R, []) ->
    undefined;
find_impl(_L, R, [{HL, _HR} | _T]) when HL =:= R + 2, R < ?MAX ->
    R + 1;
find_impl(_L, _R, [{HL, _HR} | _T]) when HL >= ?MAX ->
    undefined;
find_impl(_L, R, [{HL, HR} | T]) when HL > R ->
    find_impl(HL, HR, T);
find_impl(L, R, [{_HL, HR} | T]) ->
    find_impl(L, max(R, HR), T).
