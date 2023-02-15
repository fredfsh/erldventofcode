-module(aoc_2022_22_2).

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
            {path, path(S)};
        L ->
            string:trim(L, trailing)
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
    array:new().

do(X) ->
    X.

acc(Map, {path, Path}) ->
    {Map, Path};
acc(Map, Row) ->
    Cols = [C || {C, $#} <- lists:zip(lists:seq(0, length(Row) - 1), Row)],
    array:set(array:size(Map), sets:from_list(Cols), Map).

fin({Map, Path}) ->
    sim(0, 50, 0, 1, Path, Map).

sim(R, C, DR, DC, [], _) ->
    1000 * (R + 1) + 4 * (C + 1) + facing(DR, DC);
sim(R, C, DR, DC, [Turn | T], Map) when Turn =:= $L; Turn =:= $R ->
    {NDR, NDC} = turn(Turn, DR, DC),
    sim(R, C, NDR, NDC, T, Map);
sim(R, C, DR, DC, [0 | T], Map) ->
    sim(R, C, DR, DC, T, Map);
sim(R, C, DR, DC, [N | T], Map) ->
    {R2, C2, DR2, DC2} = move(R, C, DR, DC),
    {NR, NC, NDR, NDC} = case sets:is_element(C2, array:get(R2, Map)) of
                             true ->
                                 {R, C, DR, DC};
                             false ->
                                 {R2, C2, DR2, DC2}
                         end,
    sim(NR, NC, NDR, NDC, [N - 1 | T], Map).

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

%% Observe the following folding plan for the input:
%%
%%      0   50   100
%%           v    v
%%
%%   0       +-4-++-6-+
%%           5   ||   7
%%           +---++-3-+
%%  50       +---+
%%           2'  3'
%%           +---+
%% 100  +-2-++---+
%%      5'  ||   7'
%%      +---++-1-+
%% 150  +---+
%%      4'  1'
%%      +-6'+
%%
%% 1 -> 1'
move(149, C, 1, 0) when C >= 50 ->
    {C - 50 + 150, 49, 0, -1};
%% 1' -> 1
move(R, 49, 0, 1) when R >= 150 ->
    {149, R - 150 + 50, -1, 0};
%% 2 -> 2'
move(100, C, -1, 0) when C < 50 ->
    {C + 50, 50, 0, 1};
%% 2' -> 2
move(R, 50, 0, -1) when R >= 50, R < 100 ->
    {100, R - 50, 1, 0};
%% 3 -> 3'
move(49, C, 1, 0) when C >= 100 ->
    {C - 100 + 50, 99, 0, -1};
%% 3' -> 3
move(R, 99, 0, 1) when R >= 50, R < 100 ->
    {49, R - 50 + 100, -1, 0};
%% 4 -> 4'
move(0, C, -1, 0) when C < 100 ->
    {C - 50 + 150, 0, 0, 1};
%% 4' -> 4
move(R, 0, 0, -1) when R >= 150 ->
    {0, R - 150 + 50, 1, 0};
%% 5 -> 5'
move(R, 50, 0, -1) when R < 50 ->
    {49 - R + 100, 0, 0, 1};
%% 5' -> 5
move(R, 0, 0, -1) when R < 150 ->
    {149 - R, 50, 0, 1};
%% 6 -> 6'
move(0, C, -1, 0) when C >= 100 ->
    {199, C - 100, -1, 0};
%% 6' -> 6
move(199, C, 1, 0) ->
    {0, C + 100, 1, 0};
%% 7 -> 7'
move(R, 149, 0, 1) ->
    {49 - R + 100, 99, 0, -1};
%% 7' -> 7
move(R, 99, 0, 1) when R >= 100 ->
    {149 - R, 149, 0, -1};
move(R, C, DR, DC) ->
    {R + DR, C + DC, DR, DC}.
