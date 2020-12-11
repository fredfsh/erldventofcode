-module(aoc_2020_11_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    A = input(),
    case array:size(A) of
        0 ->
            Acc;
        _ ->
            run_impl(Acc + do(A))
    end.

input() ->
    input_impl(array:new()).

input_impl(A) ->
    case io:get_line("") of
        eof ->
            A;
        Line ->
            Line2 = string:trim(Line, trailing),
            L = array:from_list(Line2),
            input_impl(array:set(array:size(A), L, A))
    end.

do(A) ->
    do_impl(A, 0).

do_impl(A, Occupied) ->
    case change(A) of
        {_, false, _} ->
            Occupied;
        {B, true, NewOccupied} ->
            do_impl(B, NewOccupied)
    end.

change(A) ->
    change_impl(A, array:new(array:size(A)), 0, false, 0).

change_impl(A, B, I, Changed, Occupied) ->
    case array:size(A) of
        I ->
            {B, Changed, Occupied};
        _ ->
            {L, Changed2, Occupied2} = change_row(A, I),
            change_impl(A, array:set(I, L, B), I + 1,
                        Changed orelse Changed2, Occupied + Occupied2)
    end.

change_row(A, I) ->
    N = array:size(array:get(0, A)),
    change_row_impl(A, I, N, 0, array:new(N), false, 0).

change_row_impl(_A, _I, _N, _N, L, Changed, Occupied) ->
    {L, Changed, Occupied};
change_row_impl(A, I, N, J, L, Changed, Occupied) ->
    {Next, Changed2, Occupied2} =
        case {arr(A, I, J), adj(A, I, J)} of
            {$L, 0} ->
                {$#, true, 1};
            {$#, Occ} when Occ >= 5 ->
                {$L, true, 0};
            {X, _} ->
                {X, false, case X of $# -> 1; _ -> 0 end}
        end,
    change_row_impl(A, I, N, J + 1, array:set(J, Next, L),
                    Changed orelse Changed2, Occupied + Occupied2).

arr(_A, -1, _) ->
    $L;
arr(_A, _, -1) ->
    $L;
arr(A, I, J) ->
    case {array:size(A), array:size(array:get(0, A))} of
        {I, _} ->
            $L;
        {_, J} ->
            $L;
        {_, _} ->
            array:get(J, array:get(I, A))
    end.

-define(ADJ, [{-1, -1}, {-1, 0}, {-1, 1},
              { 0, -1},          { 0, 1},
              { 1, -1}, { 1, 0}, { 1, 1}]).

adj(A, I, J) ->
    F = fun({X, Y}, Acc) ->
                case trace(A, I, J, X, Y) of
                    $# ->
                        Acc + 1;
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, 0, ?ADJ).

trace(A, I, J, X, Y) ->
    case arr(A, I + X, J + Y) of
        $# ->
            $#;
        $L ->
            $L;
        _ ->
            trace(A, I +X, J + Y, X, Y)
    end.
