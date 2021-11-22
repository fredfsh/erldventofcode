-module(aoc_2018_25_1).

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
    case io:fread("", "~d,~d,~d,~d") of
        eof ->
            eof;
        {ok, [X, Y, Z, T]} ->
            {X, Y, Z, T}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    F = fun(P, {NAcc, VisitedAcc, RestAcc} = Acc) ->
                case sets:is_element(P, VisitedAcc) of
                    true ->
                        Acc;
                    false ->
                        {NewVisitedAcc, NewRestAcc} = bfs(P, VisitedAcc, RestAcc),
                        {NAcc + 1, NewVisitedAcc, NewRestAcc}
                end
        end,
    {Res, _, _} = lists:foldl(F, {0, sets:new(), sets:from_list(X)}, X),
    Res.

bfs(P, Visited, Rest) ->
    Q = queue:from_list([P]),
    bfs_impl(Q, sets:add_element(P, Visited), sets:del_element(P, Rest)).

bfs_impl(Q, Visited, Rest) ->
    case queue:out(Q) of
        {empty, _} ->
            {Visited, Rest};
        {{value, P}, Q2} ->
            F = fun(P2, {QAcc, VisitedAcc, RestAcc} = Acc) ->
                        case constellation(P, P2) of
                            true ->
                                {queue:in(P2, QAcc),
                                 sets:add_element(P2, VisitedAcc),
                                 sets:del_element(P2, RestAcc)};
                            false ->
                                Acc
                        end
                end,
            {NewQ, NewVisited, NewRest} = sets:fold(F, {Q2, Visited, Rest}, Rest),
            bfs_impl(NewQ, NewVisited, NewRest)
    end.

constellation({X1, Y1, Z1, T1}, {X2, Y2, Z2, T2}) ->
    abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2) + abs(T1 - T2) =< 3.
