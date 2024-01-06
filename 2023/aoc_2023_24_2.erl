-module(aoc_2023_24_2).

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
    case io:fread("", "~s ~s ~s @ ~s ~s ~s") of
        eof ->
            eof;
        {ok, L} ->
            [list_to_integer(string:trim(S, trailing, ",")) || S <- L]
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [list_to_tuple(X) | Acc].

%% PX + VX * Ti = PXi + VXi * Ti
%% => Ti = (PX - PXi) / (VXi - VX)
%% => (PX - PXi) / (VXi - VX) = (PY - PYi) / (VYi - VY)
%% => (PX - PXi)(VYi - VY) = (PY - PYi)(VXi - VX)
%% => PX*VYi - PX*VY - PXi*VYi + PXi*VY = PY*VXi - PY*VX - PYi*VXi + PYi*VX
%% => PX*(VYi-VYj) - (PXi*VYi-PXj*VYj) + (PXi-PXj)*VY = PY*(VXi - VXj) - (PYi*VXi-PYj*VXj) + (PYi-PYj)*VX
%% => (VYi-VYj) * PX + (VXj-VXi) * PY + (PYj-PYi) * VX + (PXi-PXj) * VY = PXi*VYi - PXj*VYj - PYi*VXi + PYj*VXj
fin(L) ->
    L2 = lists:sublist(L, 5),
    [{PXI, PYI, PZI, VXI, VYI, VZI} | L3] = L2,
    L4 = [{VYI - VYJ, VXJ - VXI, PYJ - PYI, PXI - PXJ, PXI * VYI - PXJ * VYJ - PYI * VXI + PYJ * VXJ}
          || {PXJ, PYJ, _PZJ, VXJ, VYJ, _VZJ} <- L3],
    {PX, PY, VX, _VY} = solve(L4),
    L5 = [{VZI - VZJ, VXJ - VXI, PZJ - PZI, PXI - PXJ, PXI * VZI - PXJ * VZJ - PZI * VXI + PZJ * VXJ}
          || {PXJ, _PYJ, PZJ, VXJ, _VYJ, VZJ} <- L3],
    {PX, PZ, VX, _VZ} = solve(L5),
    PX + PY + PZ.

solve([{A11, A12, A13, A14, A1},
       {A21, A22, A23, A24, A2},
       {A31, A32, A33, A34, A3},
       {A41, A42, A43, A44, A4}]) ->

    [{B11, B12, B13, B1},
     {B21, B22, B23, B2},
     {B31, B32, B33, B3}]
        = [{A11 * A24 - A21 * A14, A12 * A24 - A22 * A14, A13 * A24 - A23 * A14, A1 * A24 - A2 * A14},
           {A11 * A34 - A31 * A14, A12 * A34 - A32 * A14, A13 * A34 - A33 * A14, A1 * A34 - A3 * A14},
           {A11 * A44 - A41 * A14, A12 * A44 - A42 * A14, A13 * A44 - A43 * A14, A1 * A44 - A4 * A14}],

    [{C11, C12, C1},
     {C21, C22, C2}]
        = [{B11 * B23 - B21 * B13, B12 * B23 - B22 * B13, B1 * B23 - B2 * B13},
           {B11 * B33 - B31 * B13, B12 * B33 - B32 * B13, B1 * B33 - B3 * B13}],

    [{D11, D1}] = [{C11 * C22 - C21 * C12, C1 * C22 - C2 * C12}],

    X = D1 div D11,
    Y = (C1 - C11 * X) div C12,
    Z = (B1 - B11 * X - B12 * Y) div B13,
    W = (A1 - A11 * X - A12 * Y - A13 * Z) div A14,

    {X, Y, Z, W}.
