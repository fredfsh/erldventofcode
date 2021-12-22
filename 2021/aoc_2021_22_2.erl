-module(aoc_2021_22_2).

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
    case io:fread("", "~a x=~d..~d,y=~d..~d,z=~d..~d") of
        eof ->
            eof;
        {ok, L} ->
            L
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(RevL) ->
    L = lists:reverse(RevL),
    initialize(L) + reboot(L).

initialize(L) ->
    initialize_impl(sets:new(), L).

-define(N, 50).

initialize_impl(Ons, []) ->
    sets:size(Ons);
initialize_impl(Ons, [[_, X1, X2, Y1, Y2, Z1, Z2] | T])
  when X2 < -?N; X1 > ?N; Y2 < -?N; Y1 > ?N; Z2 < -?N; Z1 > ?N ->
    initialize_impl(Ons, T);
initialize_impl(Ons, [H | T]) ->
    initialize_impl(act(Ons, H), T).

act(Ons, [on, X1, X2, Y1, Y2, Z1, Z2]) ->
    L = [{X, Y, Z} || X <- lists:seq(X1, X2),
                      Y <- lists:seq(Y1, Y2),
                      Z <- lists:seq(Z1, Z2)],
    F = fun({X, Y, Z}, Acc) ->
                sets:add_element({X, Y, Z}, Acc)
        end,
    lists:foldl(F, Ons, L);
act(Ons, [off, X1, X2, Y1, Y2, Z1, Z2]) ->
    L = [{X, Y, Z} || X <- lists:seq(X1, X2),
                      Y <- lists:seq(Y1, Y2),
                      Z <- lists:seq(Z1, Z2)],
    F = fun({X, Y, Z}, Acc) ->
                sets:del_element({X, Y, Z}, Acc)
        end,
    lists:foldl(F, Ons, L).

reboot(Full) ->
    F = fun([_, X1, X2, Y1, Y2, Z1, Z2])
              when X2 < -?N; X1 > ?N; Y2 < -?N; Y1 > ?N; Z2 < -?N; Z1 > ?N ->
                true;
           (_) ->
                false
        end,
    L = lists:filter(F, Full),
    calc(xs(L), L).

xs(L) ->
    F = fun([_, X1, X2, _, _, _, _], Acc) ->
                [X1, X2 | Acc]
        end,
    lists:sort(lists:foldl(F, [], L)).

ys(X, L) when is_integer(X) ->
    ys({X, X}, L);
ys({Xl, Xr}, L) ->
    F = fun([_, X1, X2, Y1, Y2, _, _], Acc) when X1 =< Xl, Xr =< X2 ->
                [Y1, Y2 | Acc];
           (_, Acc) ->
                Acc
        end,
    lists:sort(lists:foldl(F, [], L)).

zs(X, Y, L) when is_integer(X) ->
    zs({X, X}, Y, L);
zs(X, Y, L) when is_integer(Y) ->
    zs(X, {Y, Y}, L);
zs({Xl, Xr}, {Yl, Yr}, L) ->
    F = fun([_, X1, X2, Y1, Y2, Z1, Z2], Acc)
              when X1 =< Xl, Xr =< X2, Y1 =< Yl, Yr =< Y2 ->
                [Z1, Z2 | Acc];
           (_, Acc) ->
                Acc
        end,
    lists:sort(lists:foldl(F, [], L)).

calc([X | T], L) ->
    Init = calc_x(X, L),
    calc_impl(Init, X, T, L).

calc_impl(Acc, _, [], _) ->
    Acc;
calc_impl(Acc, Last, [H | T], L) ->
    NewAcc = Acc + calc_x({Last, H}, L) + calc_x(H, L),
    calc_impl(NewAcc, H, T, L).

calc_x(X, L) ->
    case ys(X, L) of
        [] ->
            0;
        [Y | T] ->
            Init = calc_y(X, Y, L),
            calc_x_impl(Init, X, Y, T, L)
    end.

calc_x_impl(Acc, _, _, [], _) ->
    Acc;
calc_x_impl(Acc, X, Last, [H | T], L) ->
    NewAcc = Acc + calc_y(X, {Last, H}, L) + calc_y(X, H, L),
    calc_x_impl(NewAcc, X, H, T, L).

calc_y(X, Y, L) ->
    case zs(X, Y, L) of
        [] ->
            0;
        [Z | T] ->
            Init = calc_z(X, Y, Z, L),
            calc_y_impl(Init, X, Y, Z, T, L)
    end.

calc_y_impl(Acc, _, _, _, [], _) ->
    Acc;
calc_y_impl(Acc, X, Y, Last, [H | T], L) ->
    NewAcc = Acc + calc_z(X, Y, {Last, H}, L) + calc_z(X, Y, H, L),
    calc_y_impl(NewAcc, X, Y, H, T, L).

calc_z(X, Y, Z, L) ->
    F = fun([Act, X1, X2, Y1, Y2, Z1, Z2], Acc) ->
                case within(X, Y, Z, {X1, X2, Y1, Y2, Z1, Z2}) of
                    true ->
                        Act;
                    false ->
                        Acc
                end
        end,
    case lists:foldl(F, off, L) of
        on ->
            count(X, Y, Z);
        off ->
            0
    end.

within(X, Y, Z, Range) when is_integer(X) ->
    within({X, X}, Y, Z, Range);
within(X, Y, Z, Range) when is_integer(Y) ->
    within(X, {Y, Y}, Z, Range);
within(X, Y, Z, Range) when is_integer(Z) ->
    within(X, Y, {Z, Z}, Range);
within({Xl, Xr}, {Yl, Yr}, {Zl, Zr}, {X1, X2, Y1, Y2, Z1, Z2}) ->
    true
        andalso Xl >= X1 andalso Xr =< X2
        andalso Yl >= Y1 andalso Yr =< Y2
        andalso Zl >= Z1 andalso Zr =< Z2.

count(X, Y, Z) ->
    count(X) * count(Y) * count(Z).

count({X1, X2}) ->
    X2 - X1 - 1;
count(_) ->
    1.
