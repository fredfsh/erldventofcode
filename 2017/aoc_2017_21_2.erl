-module(aoc_2017_21_2).

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
    case io:fread("", "~s => ~s") of
        eof ->
            eof;
        {ok, [In, Out]} ->
            {string:split(In, "/", all), string:split(Out, "/", all)}
    end.

ini() ->
    maps:new().

do(X) ->
    X.

-define(TR, [fun id/1,
             fun r90/1,
             fun r180/1,
             fun r270/1,
             fun flip/1,
             fun fr90/1,
             fun fr180/1,
             fun fr270/1]).

acc(Acc, {In, Out}) ->
    F = fun(Fun, MapAcc) ->
                maps:put(Fun(In), Out, MapAcc)
        end,
    lists:foldl(F, Acc, ?TR).

id(X) ->
    X.

r90([[A,B],
     [C,D]]) ->
    [[C,A],
     [D,B]];
r90([[A,B,C],
     [D,E,F],
     [G,H,I]]) ->
    [[G,D,A],
     [H,E,B],
     [I,F,C]].

r180(X) ->
    r90(r90(X)).

r270(X) ->
    r90(r180(X)).

flip(X) ->
    [lists:reverse(L) || L <- X].

fr90(X) ->
    flip(r90(X)).

fr180(X) ->
    flip(r180(X)).

fr270(X) ->
    flip(r270(X)).

-define(INIT, [".#.",
               "..#",
               "###"]).

fin(X) ->
    expand(0, ?INIT, X).

-define(N, 18).

expand(?N, Acc, _) ->
    on(Acc);
expand(I, Acc, Map) ->
    Size = case length(Acc) rem 2 of
               0 ->
                   2;
               _ ->
                   3
           end,
    Acc2 = break_down(Acc, Size),
    Acc3 = transform(Acc2, Map),
    Acc4 = group_up(Acc3),
    expand(I + 1, Acc4, Map).

break_down(M, Size) ->
    break_down_cols(break_down_rows([[]], M, Size), Size).

break_down_rows([H | T], [], _) ->
    lists:reverse([lists:reverse(H) | T]);
break_down_rows([H | T], [MH | MT], Size) when length(H) =:= Size ->
    break_down_rows([[MH], lists:reverse(H) | T], MT, Size);
break_down_rows([H | T], [MH | MT], Size) ->
    break_down_rows([[MH | H] | T], MT, Size).

break_down_cols(L, Size) ->
    F = fun(X) -> break_down_cols_impl([], X, Size) end,
    lists:map(F, L).

break_down_cols_impl(Acc, [[] | _], _) ->
    lists:reverse(Acc);
break_down_cols_impl(Acc, [[A, B | T1], [C, D | T2]], 2) ->
    break_down_cols_impl([[[A, B], [C, D]] | Acc], [T1, T2], 2);
break_down_cols_impl(Acc, [[A, B, C | T1], [D, E, F | T2], [G, H, I | T3]], 3) ->
    break_down_cols_impl([[[A,B,C], [D,E,F], [G,H,I]] | Acc], [T1, T2, T3], 3).

transform(L, Map) ->
    [transform_row(X, Map) || X <- L].

transform_row(L, Map) ->
    [maps:get(X, Map) || X <- L].

group_up(L) ->
    group_up_rows(group_up_cols(L)).

group_up_cols(L) ->
    N = length(hd(hd(L))),
    F = fun(X) -> group_up_cols_impl(lists:duplicate(N, []), X) end,
    lists:map(F, L).

group_up_cols_impl(Acc, []) ->
    Acc;
group_up_cols_impl([L1, L2, L3], [[R1, R2, R3] | T]) ->
    group_up_cols_impl([L1 ++ R1, L2 ++ R2, L3 ++ R3], T);
group_up_cols_impl([L1, L2, L3, L4], [[R1, R2, R3, R4] | T]) ->
    group_up_cols_impl([L1 ++ R1, L2 ++ R2, L3 ++ R3, L4 ++ R4], T).

group_up_rows(L) ->
    lists:append(L).

on(L) when is_list(L) ->
    lists:sum([on(X) || X <- L]);
on($#) ->
    1;
on($.) ->
    0.
