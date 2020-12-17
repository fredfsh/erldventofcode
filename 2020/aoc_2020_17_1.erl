-module(aoc_2020_17_1).

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
    A = init_cube(N),
    B = array:set(0, X, A),
%%    io:format("Init: ~p~n", [B]),
    do_impl(B, 6).

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
    Fy = fun(_Y, C, Accy) ->
                 array:foldl(Fx, Accy, C)
         end,
    Fz = fun(_Z, B, Accz) ->
                 array:foldl(Fy, Accz, B)
         end,
    array:foldl(Fz, 0, A).

init_array(A) ->
    N = array:size(A),
    init_cube(N + 2).

init_cube(N) ->
    A = array:new(N),
    init_cube_impl(A, N - 1, N).

init_cube_impl(A, -1, _) ->
    A;
init_cube_impl(A, Z, N) ->
    B = init_slice(N),
    init_cube_impl(array:set(Z, B, A), Z - 1, N).

init_slice(N) ->
    B = array:new(N),
    init_slice_impl(B, N - 1, N).

init_slice_impl(B, -1, _) ->
    B;
init_slice_impl(B, Y, N) ->
    C = array:new(N, {default, $.}),
    init_slice_impl(array:set(Y, C, B), Y - 1, N).

transform(A, A2) ->
    N = array:size(A2),
    transform_impl(A2, A, N, N - 1, N - 1, N - 1).

transform_impl(A2, _A, _N, -1, _Y, _X) ->
    A2;
transform_impl(A2, A, N, Z, Y, -1) ->
    transform_impl(A2, A, N, Z, Y - 1, N - 1);
transform_impl(A2, A, N, Z, -1, _) ->
    transform_impl(A2, A, N, Z - 1, N - 1, N - 1);
transform_impl(A2, A, N, Z, Y, X) ->
    State = case {arr(A, X - 1, Y - 1, Z - 1),
                  adj(A, X - 1, Y - 1, Z - 1)} of
                {$#, 2} ->
                    $#;
                {$#, 3} ->
                    $#;
                {$#, _} ->
                    $.;
                {$., 3} ->
                    $#;
                {$., _} ->
                    $.
            end,
    transform_impl(arr_s(A2, X, Y, Z, State), A, N, Z, Y, X - 1).


arr(A, X, Y, Z) ->
    N = array:size(A),
    case (X =< -1 orelse Y =< -1 orelse Z =< -1 orelse
          X >= N orelse Y >= N orelse Z >= N) of
        true ->
            $.;
        _ ->
            array:get(X, array:get(Y, array:get(Z, A)))
    end.

-define(NEIGHBORS, [
                    [-1, -1, -1],
                    [-1, -1,  0],
                    [-1, -1,  1],
                    [-1,  0, -1],
                    [-1,  0,  0],
                    [-1,  0,  1],
                    [-1,  1, -1],
                    [-1,  1,  0],
                    [-1,  1,  1],

                    [ 0, -1, -1],
                    [ 0, -1,  0],
                    [ 0, -1,  1],
                    [ 0,  0, -1],
                    [ 0,  0,  1],
                    [ 0,  1, -1],
                    [ 0,  1,  0],
                    [ 0,  1,  1],

                    [ 1, -1, -1],
                    [ 1, -1,  0],
                    [ 1, -1,  1],
                    [ 1,  0, -1],
                    [ 1,  0,  0],
                    [ 1,  0,  1],
                    [ 1,  1, -1],
                    [ 1,  1,  0],
                    [ 1,  1,  1]
                   ]).

adj(A, X, Y, Z) ->
    F = fun([DX, DY, DZ], Acc) ->
                case arr(A, X + DX, Y + DY, Z + DZ) of
                    $# ->
                        Acc + 1;
                    $. ->
                        Acc
                end
        end,
    lists:foldl(F, 0, ?NEIGHBORS).


arr_s(A, X, Y, Z, Val) ->
    B = array:get(Z, A),
    C = array:get(Y, B),
    C2 = array:set(X, Val, C),
    B2 = array:set(Y, C2, B),
    array:set(Z, B2, A).
