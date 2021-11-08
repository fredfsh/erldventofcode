-module(aoc_2017_20_2).

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
    case io:fread("", "p=<~d,~d,~d>, v=<~d,~d,~d>, a=<~d,~d,~d>") of
        eof ->
            eof;
        {ok, [PX, PY, PZ, VX, VY, VZ, AX, AY, AZ]} ->
            {{PX, PY, PZ}, {VX, VY, VZ}, {AX, AY, AZ}}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    N = length(X),
    P = lists:zip(X, lists:seq(1, N)),
    L = [collision(M1, M2, I1, I2) || {M1, I1} <- P, {M2, I2} <- P, I1 < I2],
    remove(N, lists:append(L)).

collision(M1, M2, ID1, ID2) ->
    [{X, ID1, ID2} || X <- collision_time(M1, M2)].

collision_time({{PX1, _, _}, {VX1, _, _}, {AX1, _, _}} = P1,
               {{PX2, _, _}, {VX2, _, _}, {AX2, _, _}} = P2) ->
    Candidates = solve(PX1, VX1, AX1, PX2, VX2, AX2),
    lists:filter(fun(T) -> validate(T, P1, P2) end, Candidates).

solve(P1, V1, A1, P2, V2, A2) when A1 =:= A2, V1 =:= V2, P1 =:= P2 ->
    [0];
solve(_P1, V1, A1, _P2, V2, A2) when A1 =:= A2, V1 =:= V2 ->
    [];
solve(P1, V1, A1, P2, V2, A2) when A1 =:= A2 ->
    [(P2 - P1) / (V1 - V2)];
solve(P1, V1, A1, P2, V2, A2) ->
    A = A1 - A2,
    B = 2 * V1 + A1 - 2 * V2 - A2,
    C = 2 * P1 - 2 * P2,
    Candidates = case B * B - 4 * A * C of
                     Neg when Neg < 0 ->
                         [];
                     0 ->
                         [-B / 2 / A];
                     Pos ->
                         Root = math:sqrt(Pos),
                         [(-B + Root) / 2 / A, (-B - Root) / 2 / A]
                 end,
    lists:filter(fun(T) -> T >= 0 end, Candidates).

validate(T,
         {{_, PY1, PZ1}, {_, VY1, VZ1}, {_, AY1, AZ1}},
         {{_, PY2, PZ2}, {_, VY2, VZ2}, {_, AY2, AZ2}}) ->
    validate(T, PY1, VY1, AY1, PY2, VY2, AY2)
        andalso validate(T, PZ1, VZ1, AZ1, PZ2, VZ2, AZ2).

validate(T, P1, V1, A1, P2, V2, A2) ->
    A1*T*T + (2*V1+A1)*T + 2*P1 =:= A2*T*T + (2*V2+A2)*T + 2*P2.


remove(N, L) ->
    remove_impl(maps:new(), lists:keysort(1, L), N).

remove_impl(Removed, [], N) ->
    N - maps:size(Removed);
remove_impl(Removed, [{Time, ID1, ID2} | T], N) ->
    NewRemoved = case {maps:is_key(ID1, Removed), maps:is_key(ID2, Removed)} of
                     {false, false} ->
                         maps:put(ID2, Time, maps:put(ID1, Time, Removed));
                     {false, true} ->
                         case maps:get(ID2, Removed) of
                             Time ->
                                 maps:put(ID1, Time, Removed);
                             _ ->
                                 Removed
                         end;
                     {true, false} ->
                         case maps:get(ID1, Removed) of
                             Time ->
                                 maps:put(ID2, Time, Removed);
                             _ ->
                                 Removed
                         end;
                     {true, true} ->
                         Removed
                 end,
    remove_impl(NewRemoved, T, N).
