-module(aoc_2022_22_1).

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
    case io:get_line("") of
        eof ->
            eof;
        "\n" ->
            {ok, [S]} = io:fread("", "~s"),
            path(S);
        L ->
            array:from_list(string:trim(L, trailing))
    end.

path(S) ->
    path_impl([], 0, S).

path_impl(Acc, N, []) ->
    lists:reverse([N | Acc]);
path_impl(Acc, N, [D | T]) when D =:= $L; D =:= $R ->
    path_impl([D, N | Acc], 0, T);
path_impl(Acc, N, [X | T]) ->
    path_impl(Acc, N * 10 + X - $0, T).

ini() ->
    {array:new(), array:new(), maps:new()}.

do(X) ->
    X.

-define(SPACE, 16#20).

acc({Map, RowLimits, ColLimits}, Path) when is_list(Path) ->
    {Map, RowLimits, ColLimits, Path};
acc({Map, RowLimits, ColLimits}, Row) ->
    Y = array:size(Map),
    G = fun({L, _}) -> {L, Y} end,
    F = fun(_, ?SPACE, Acc) ->
                Acc;
           (X, $., {WAcc, MXAcc, CLAcc}) ->
                {WAcc, min(MXAcc, X), maps:update_with(X, G, {Y, Y}, CLAcc)};
           (X, $#, {WAcc, MXAcc, CLAcc}) ->
                {sets:add_element(X, WAcc),
                 min(MXAcc, X),
                 maps:update_with(X, G, {Y, Y}, CLAcc)}
        end,
    {Walls, MinX, NColLimits} = array:foldl(F, {sets:new(), u, ColLimits}, Row),
    NRowLimits = array:set(Y, {MinX, array:size(Row) - 1}, RowLimits),
    {array:set(Y, Walls, Map), NRowLimits, NColLimits}.

fin({Map, RowLimits, ColLimits, Path}) ->
    {C, _} = array:get(0, RowLimits),
    sim(0, C, 0, 1, Path, RowLimits, ColLimits, Map).

sim(R, C, DR, DC, [], _, _, _) ->
    1000 * (R + 1) + 4 * (C + 1) + facing(DR, DC);
sim(R, C, DR, DC, [Turn | T], RL, CL, Map) when Turn =:= $L; Turn =:= $R ->
    {NDR, NDC} = turn(Turn, DR, DC),
    sim(R, C, NDR, NDC, T, RL, CL, Map);
sim(R, C, DR, DC, [0 | T], RL, CL, Map) ->
    sim(R, C, DR, DC, T, RL, CL, Map);
sim(R, C, DR, DC, [N | T], RL, CL, Map) ->
    {R2, C2} = move(R, C, DR, DC, RL, CL),
    {NR, NC} = case sets:is_element(C2, array:get(R2, Map)) of
                   true ->
                       {R, C};
                   false ->
                       {R2, C2}
               end,
    sim(NR, NC, DR, DC, [N - 1 | T], RL, CL, Map).

facing( 0,  1) -> 0;
facing( 1,  0) -> 1;
facing( 0, -1) -> 2;
facing(-1,  0) -> 3.

turn($L,  0,  1) ->
    {-1,  0};
turn($L,  1,  0) ->
    { 0,  1};
turn($L,  0, -1) ->
    { 1,  0};
turn($L, -1,  0) ->
    { 0, -1};
turn($R,  0,  1) ->
    { 1,  0};
turn($R,  1,  0) ->
    { 0, -1};
turn($R,  0, -1) ->
    {-1,  0};
turn($R, -1,  0) ->
    { 0,  1}.

move(R, C, 0, 1, RowLimits, _) ->
    case array:get(R, RowLimits) of
        {Min, C} ->
            {R, Min};
        _ ->
            {R, C + 1}
    end;
move(R, C, 1, 0, _, ColLimits) ->
    case maps:get(C, ColLimits) of
        {Min, R} ->
            {Min, C};
        _ ->
            {R + 1, C}
    end;
move(R, C, 0, -1, RowLimits, _) ->
    case array:get(R, RowLimits) of
        {C, Max} ->
            {R, Max};
        _ ->
            {R, C - 1}
    end;
move(R, C, -1, 0, _, ColLimits) ->
    case maps:get(C, ColLimits) of
        {R, Max} ->
            {Max, C};
        _ ->
            {R - 1, C}
    end.
