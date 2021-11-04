-module(aoc_2017_14_2).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

-define(ROWS, 128).

do(X) ->
    F = fun(I, Acc) ->
                In = lists:append([X, "-", integer_to_list(I)]),
                Hash = aoc_2017_10_2:do(In),
                add_used(Acc, I, 0, Hash)
        end,
    Used = lists:foldl(F, sets:new(), lists:seq(0, ?ROWS - 1)),
    regions(Used).

add_used(Set, _, _, []) ->
    Set;
add_used(Set, Y, X, [H | T]) ->
    N = dehex(H),
    Set2 = case N band 2#1000 of
               0 ->
                   Set;
               _ ->
                   sets:add_element({X, Y}, Set)
           end,
    Set3 = case N band 2#100 of
               0 ->
                   Set2;
               _ ->
                   sets:add_element({X + 1, Y}, Set2)
           end,
    Set4 = case N band 2#10 of
               0 ->
                   Set3;
               _ ->
                   sets:add_element({X + 2, Y}, Set3)
           end,
    Set5 = case N band 2#1 of
               0 ->
                   Set4;
               _ ->
                   sets:add_element({X + 3, Y}, Set4)
           end,
    add_used(Set5, Y, X + 4, T).

dehex(C) when C >= $0, C =< $9 ->
    C - $0;
dehex(C) ->
    C - $a + 10.

regions(Set) ->
    regions_impl(0, Set).

regions_impl(Acc, Set) ->
    case sets:to_list(Set) of
        [] ->
            Acc;
        [{X, Y} | _] ->
            Q = queue:in({X, Y}, queue:new()),
            regions_impl(Acc + 1, floodfill(Q, sets:del_element({X, Y}, Set)))
    end.

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

floodfill(Q, Set) ->
    case queue:out(Q) of
        {empty, _} ->
            Set;
        {{value, {X, Y}}, Q2} ->
            Candidates = [{X + DX, Y + DY} || {DX, DY} <- ?D],
            F = fun({FX, FY}, {QAcc, SetAcc} = Acc) ->
                        case sets:is_element({FX, FY}, SetAcc) of
                            true ->
                                {queue:in({FX, FY}, QAcc),
                                 sets:del_element({FX, FY}, SetAcc)};
                            false ->
                                Acc
                        end
                end,
            {NewQ, NewSet} = lists:foldl(F, {Q2, Set}, Candidates),
            floodfill(NewQ, NewSet)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
