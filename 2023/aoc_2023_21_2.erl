-module(aoc_2023_21_2).

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
        {ok, [L]} ->
            array:from_list(L)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(STEPS, 26501365).

%% Observe map expands T times in each direction, where T is 202300.
%%
%% Observe reached garden plots must be in one of the four areas:
%% 1) central diamond with the different parity as original map (Diamond')
%% 2) central diamond with the same parity as original map (Diamond)
%% 3) four corners with the same parity as original map (Complement)
%% 4) four corners with the different parity as original map (Complement')
%%
%%   T     Diamond'         Diamond     Complement       Complement'
%% -----------------------------------------------------------------
%%   0        1                0             0                0
%%   1        1                4             2                2
%%   2        9                4             6                6
%%   3        9               16            12               12
%%
%%   T   (Tdiv2*2+1)^2  ((T+1)div2*2)^2    T(T+1)           T(T+1)
fin(Matrix) ->
    Unreachable = unreachable(Matrix),
    N = array:size(Matrix),
    T = (?STEPS - N div 2) div N,
    DP = (T div 2 * 2 + 1) * (T div 2 * 2 + 1),
    D = ((T + 1) div 2 * 2) * ((T + 1) div 2 * 2),
    C = T * (T + 1),
    lists:sum([diamond(Unreachable, Matrix, 1) * DP,
               diamond(Unreachable, Matrix, 0) * D,
               complement(Unreachable, Matrix) * C]).

unreachable(Matrix) ->
    Reachable = floodfill(Matrix),
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $., GAcc) ->
                            XY = {X, Y},
                            case sets:is_element(XY, Reachable) of
                                false ->
                                    sets:add_element(XY, GAcc);
                                true ->
                                    GAcc
                            end;
                       (_, _, GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, sets:new(), Matrix).

floodfill(Matrix) ->
    floodfill_impl(sets:new(), queue:from_list([{0, 0}]), Matrix).

-define(Ds, [{1, 0}, {0, -1}, {-1, 0}, {0, 1}]).
-define(WALL(X, Y), (array:get(X, array:get(Y, Matrix)) =:= $#)).

floodfill_impl(Acc, Q, Matrix) ->
    case queue:out(Q) of
        {empty, _} ->
            Acc;
        {{value, {X, Y}}, Q2} ->
            Rows = array:size(Matrix),
            Cols = array:size(array:get(0, Matrix)),
            F = fun({DX, DY}, {ReachableAcc, QAcc} = FAcc) ->
                        case {X + DX, Y + DY} of
                            {NX, _} when NX < 0; NX >= Cols ->
                                FAcc;
                            {_, NY} when NY < 0; NY >= Rows ->
                                FAcc;
                            {NX, NY} = NXY ->
                                case sets:is_element(NXY, ReachableAcc)
                                    orelse ?WALL(NX, NY) of
                                    true ->
                                        FAcc;
                                    false ->
                                        {sets:add_element(NXY, ReachableAcc),
                                         queue:in(NXY, QAcc)}
                                end
                        end
                end,
            {NAcc, NQ} = lists:foldl(F, {Acc, Q2}, ?Ds),
            floodfill_impl(NAcc, NQ, Matrix)
    end.

diamond(Unreachable, Matrix, Parity) ->
    N = array:size(Matrix),
    M = N div 2,

    F = fun(Y, Arr, FAcc) ->
                G = fun(_, $#, GAcc) ->
                            GAcc;
                       (X, _, GAcc) when abs(X - M) + abs(Y - M) =< M andalso
                                         (X + Y) rem 2 =:= Parity ->
                            case sets:is_element({X, Y}, Unreachable) of
                                true ->
                                    GAcc;
                                false ->
                                    GAcc + 1
                            end;
                       (_, _, GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, 0, Matrix).

complement(Unreachable, Matrix) ->
    N = array:size(Matrix),
    M = N div 2,

    F = fun(Y, Arr, FAcc) ->
                G = fun(_, $#, GAcc) ->
                            GAcc;
                       (X, _, GAcc) when abs(X - M) + abs(Y - M) > M ->
                            case sets:is_element({X, Y}, Unreachable) of
                                true ->
                                    GAcc;
                                false ->
                                    GAcc + 1
                            end;
                       (_, _, GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, 0, Matrix).
