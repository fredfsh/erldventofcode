-module(aoc_2017_12_1).

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
    case io:fread("", "~d <->") of
        eof ->
            eof;
        {ok, [X]} ->
            L = io:get_line(""),
            Parts = string:split(string:trim(L), ", ", all),
            {X, [list_to_integer(P) || P <- Parts]}
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, {X, Ys}) ->
    maps:put(X, sets:from_list(Ys), Acc).

fin(X) ->
    Q = queue:in(0, queue:new()),
    Visited = sets:add_element(0, sets:new()),
    bfs(Q, Visited, X).

bfs(Q, Visited, X) ->
    case queue:out(Q) of
        {empty, _} ->
            sets:size(Visited);
        {{value, ID}, Q2} ->
            Dsts = maps:get(ID, X),
            F = fun(Dst, {QAcc, VisitedAcc} = Acc) ->
                        case sets:is_element(Dst, VisitedAcc) of
                            true ->
                                Acc;
                            false ->
                                {queue:in(Dst, QAcc),
                                 sets:add_element(Dst, VisitedAcc)}
                        end
                end,
            {NewQ, NewVisited} = sets:fold(F, {Q2, Visited}, Dsts),
            bfs(NewQ, NewVisited, X)
    end.
