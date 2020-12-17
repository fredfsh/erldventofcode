-module(aoc_2020_17_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    X = input(),
    case array:size(X) of
        0 ->
            Acc;
        _ ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl(array:new()).

input_impl(A) ->
    case io:get_line("") of
        eof ->
            A;
        L ->
            L2 = string:trim(L, trailing),
            A2 = array:set(array:size(A), array:from_list(L2), A),
            input_impl(A2)
    end.

do(X) ->
    N = array:size(X),
    A = init_hypercube(1, 1, N, N),
    B = init_cube(1, N, N),
    B2 = array:set(0, X, B),
    A2 = array:set(0, B2, A),
%%    io:format("Init: ~p~n", [A2]),
    do_impl(A2, 6).

do_impl(A, 0) ->
    count_active(A);
do_impl(A, CD) ->
    A2 = init_array(A),
    A3 = transform(A, A2),
%%    io:format("Turn: ~p~n~p~n", [CD, A3]),
    do_impl(A3, CD - 1).

count_active(A) ->
    Fx = fun(_X, $#, Accx) ->
                 Accx + 1;
            (_X, $., Accx) ->
                 Accx
         end,
    Fy = fun(_Y, D, Accy) ->
                 array:foldl(Fx, Accy, D)
         end,
    Fz = fun(_Z, C, Accz) ->
                 array:foldl(Fy, Accz, C)
         end,
    Fw = fun(_W, B, Accw) ->
                 array:foldl(Fz, Accw, B)
         end,
    array:foldl(Fw, 0, A).

init_array(A) ->
    {NW, NZ, NY, NX} = dim(A),
    init_hypercube(NW + 2, NZ + 2, NY + 2, NX + 2).

dim(A) ->
    NW = array:size(A),
    B = array:get(0, A),
    NZ = array:size(B),
    C = array:get(0, B),
    NY = array:size(C),
    D = array:get(0, C),
    NX = array:size(D),
    {NW, NZ, NY, NX}.


init_hypercube(NW, NZ, NY, NX) ->
    A = array:new(NW),
    init_hypercube_impl(A, NW - 1, NZ, NY, NX).

init_hypercube_impl(A, -1, _, _, _) ->
    A;
init_hypercube_impl(A, W, NZ, NY, NX) ->
    B = init_cube(NZ, NY, NX),
    init_hypercube_impl(array:set(W, B, A), W - 1, NZ, NY, NX).

init_cube(NZ, NY, NX) ->
    A = array:new(NZ),
    init_cube_impl(A, NZ - 1, NY, NX).

init_cube_impl(A, -1, _, _) ->
    A;
init_cube_impl(A, Z, NY, NX) ->
    B = init_slice(NY, NX),
    init_cube_impl(array:set(Z, B, A), Z - 1, NY, NX).

init_slice(NY, NX) ->
    B = array:new(NY),
    init_slice_impl(B, NY - 1, NX).

init_slice_impl(B, -1, _) ->
    B;
init_slice_impl(B, Y, NX) ->
    C = array:new(NX, {default, $.}),
    init_slice_impl(array:set(Y, C, B), Y - 1, NX).

transform(A, A2) ->
    Dim = dim(A2),
    {NW, NZ, NY, NX} = Dim,
    transform_impl(A2, A, Dim, NW - 1, NZ - 1, NY - 1, NX - 1).

transform_impl(A2, _A, _Dim, -1, _Z, _Y, _X) ->
    A2;
transform_impl(A2, A, {_NW, _NZ, _NY, NX} = Dim, W, Z, Y, -1) ->
    transform_impl(A2, A, Dim, W, Z, Y - 1, NX - 1);
transform_impl(A2, A, {_NW, _NZ, NY, NX} = Dim, W, Z, -1, _) ->
    transform_impl(A2, A, Dim, W, Z - 1, NY - 1, NX - 1);
transform_impl(A2, A, {_NW, NZ, NY, NX} = Dim, W, -1, _, _) ->
    transform_impl(A2, A, Dim, W - 1, NZ - 1, NY - 1, NX - 1);
transform_impl(A2, A, Dim, W, Z, Y, X) ->
    State = case {arr(A, X - 1, Y - 1, Z - 1, W - 1),
                  adj(A, X - 1, Y - 1, Z - 1, W - 1)} of
                {$#, 3} ->
                    $#;
                {$#, 4} ->
                    $#;
                {$#, _} ->
                    $.;
                {$., 3} ->
                    $#;
                {$., _} ->
                    $.
            end,
    transform_impl(arr_s(A2, X, Y, Z, W, State), A, Dim, W, Z, Y, X - 1).


arr(A, X, Y, Z, W) ->
    {NW, NZ, NY, NX} = dim(A),
    case (X =< -1 orelse Y =< -1 orelse Z =< -1 orelse W =< -1 orelse
          X >= NX orelse Y >= NY orelse Z >= NZ orelse W >= NW) of
        true ->
            $.;
        _ ->
            array:get(X, array:get(Y, array:get(Z, array:get(W, A))))
    end.

adj(A, X, Y, Z, W) ->
    F = fun(Bitmap, Acc) ->
                {DX, DY, DZ, DW} = decode(Bitmap),
                case arr(A, X + DX, Y + DY, Z + DZ, W + DW) of
                    $# ->
                        Acc + 1;
                    $. ->
                        Acc
                end
        end,
    lists:foldl(F, 0, lists:seq(0, 80)).

decode(N) ->
    DX = N rem 3 - 1,
    DY = N div 3 rem 3 - 1,
    DZ = N div 9 rem 3 - 1,
    DW = N div 27 - 1,
    {DX, DY, DZ, DW}.

arr_s(A, X, Y, Z, W, Val) ->
    B = array:get(W, A),
    C = array:get(Z, B),
    D = array:get(Y, C),
    D2 = array:set(X, Val, D),
    C2 = array:set(Y, D2, C),
    B2 = array:set(Z, C2, B),
    array:set(W, B2, A).
