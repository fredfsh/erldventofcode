-module(aoc_2016_24_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(array:new(), []).

input_impl(Arr, L) ->
    case io:fread("", "~s") of
        eof ->
            {Arr, L};
        {ok, [Line]} ->
            Inner = array:from_list(Line),
            input_impl(array:set(array:size(Arr), Inner, Arr),
                       input_stops(Inner, L, array:size(Arr)))
    end.

input_stops(Arr, L, Y) ->
    F = fun(I, C, Acc) when C >= $0, C =< $9 ->
                [{C, {I, Y}} | Acc];
           (_, _, Acc) ->
                Acc
        end,
    array:foldl(F, L, Arr).

do({Arr, L}) ->
    Graph = graph(Arr, L),
    dfs(Graph).

graph(Arr, L) ->
    maps:from_list([{{A, B}, floodfill(AXY, BXY, Arr)}
                    || {A, AXY} <- L, {B, BXY} <- L, A < B]).

floodfill(Start, Target, Arr) ->
    Q = queue:in({Start, 0}, queue:new()),
    Visited = sets:add_element(Start, sets:new()),
    floodfill_impl(Q, Visited, Target, Arr).

floodfill_impl(Q, Visited, Target, Arr) ->
    {{value, {XY, N}}, Q2} = queue:out(Q),
    Candidates = neighbors(XY),
    F = fun(Next, _) when Next =:= Target ->
                N + 1;
           ({X, Y}, {QAcc, VisitedAcc}) ->
                case
                    X >= 0
                    andalso Y >= 0
                    andalso X < array:size(array:get(0, Arr))
                    andalso Y < array:size(Arr)
                    andalso array:get(X, array:get(Y, Arr)) =/= $#
                    andalso not sets:is_element({X, Y}, VisitedAcc)
                of
                    true ->
                        {queue:in({{X, Y}, N + 1}, QAcc),
                         sets:add_element({X, Y}, VisitedAcc)};
                    false ->
                        {QAcc, VisitedAcc}
                end;
           (_, M) ->
                M
        end,
    case lists:foldl(F, {Q2, Visited}, Candidates) of
        {NewQ, NewVisited} ->
            floodfill_impl(NewQ, NewVisited, Target, Arr);
        M ->
            M
    end.

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

neighbors({X, Y}) ->
    [{X + DX, Y + DY} || {DX, DY} <- ?D].

dfs(Graph) ->
    Digits = digits(Graph),
    dfs_impl(0, $0, sets:del_element($0, Digits), Graph).

digits(Graph) ->
    F = fun({X, Y}, _, Acc) ->
                sets:add_element(X, sets:add_element(Y, Acc))
        end,
    maps:fold(F, sets:new(), Graph).

dfs_impl(Cur, Last, Remaining, Graph) ->
    case sets:is_empty(Remaining) of
        true ->
            Cur + dist($0, Last, Graph);
        false ->
            F = fun(Next, Min) ->
                        min(Min, dfs_impl(Cur + dist(Last, Next, Graph),
                                          Next,
                                          sets:del_element(Next, Remaining),
                                          Graph))
                end,
            sets:fold(F, undefined, Remaining)
    end.

dist(A, B, Graph) when A > B ->
    dist(B, A, Graph);
dist(A, B, Graph) ->
    maps:get({A, B}, Graph).
