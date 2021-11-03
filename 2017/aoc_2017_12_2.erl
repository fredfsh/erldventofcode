-module(aoc_2017_12_2).

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
    groups(0, X).

groups(Acc, Map) ->
    case maps:size(Map) of
        0 ->
            Acc;
        _ ->
            ID = hd(maps:keys(Map)),
            groups(Acc + 1, group(ID, Map))
    end.

group(ID, Map) ->
    Q = queue:in(ID, queue:new()),
    bfs(Q, Map).

bfs(Q, Map) ->
    case queue:out(Q) of
        {empty, _} ->
            Map;
        {{value, ID}, Q2} ->
            Dsts = maps:get(ID, Map, sets:new()),
            F = fun(Dst, QAcc) ->
                        case maps:is_key(Dst, Map) of
                            false ->
                                QAcc;
                            true ->
                                queue:in(Dst, QAcc)
                        end
                end,
            NewQ = sets:fold(F, Q2, Dsts),
            bfs(NewQ, maps:remove(ID, Map))
    end.
