-module(aoc_2018_7_2).

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
    case io:fread("", "Step ~c must be finished before step ~c can begin.") of
        eof ->
            eof;
        {ok, [[From], [To]]} ->
            {From, To}
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Map, {From, To}) ->
    add_post(From, To, add_pre(To, From, Map)).

add_pre(K, V, Map) ->
    {Pres, Posts} = maps:get(K, Map, {sets:new(), sets:new()}),
    maps:put(K, {sets:add_element(V, Pres), Posts}, Map).

add_post(K, V, Map) ->
    {Pres, Posts} = maps:get(K, Map, {sets:new(), sets:new()}),
    maps:put(K, {Pres, sets:add_element(V, Posts)}, Map).

-define(WORKERS, 5).

fin(Map) ->
    Init = maps:keys(maps:filter(fun(_, {P, _}) -> sets:size(P) =:= 0 end, Map)),
    sim(0, lists:sort(Init), [], Map).

-define(BASE, 60).

sim(Acc, [], [], _) ->
    Acc;
sim(Acc, [H | T], Workers, Map) when length(Workers) < ?WORKERS ->
    sim(Acc, T, [{H, H - $A + 1 + ?BASE} | Workers], Map);
sim(Acc, Jobs, Workers, Map) ->
    {NewJobs, NewWorkers, NewMap} = work(Jobs, Workers, Map),
    sim(Acc + 1, lists:sort(NewJobs), NewWorkers, NewMap).

work(Jobs, Workers, Map) ->
    F = fun({Job, 1}, {JobsAcc, WorkersAcc, MapAcc}) ->
                {NewJobsAcc, NewMapAcc} = done(Job, MapAcc, JobsAcc),
                {NewJobsAcc, WorkersAcc, NewMapAcc};
           ({Job, ETA}, {JobsAcc, WorkersAcc, MapAcc}) ->
                {JobsAcc, [{Job, ETA - 1} | WorkersAcc], MapAcc}
        end,
    lists:foldl(F, {Jobs, [], Map}, Workers).

done(Job, Map, Jobs) ->
    {_, Posts} = maps:get(Job, Map),
    F = fun(To, {JobsAcc, MapAcc}) ->
                {Pres, KPosts} = maps:get(To, MapAcc),
                NewPres = sets:del_element(Job, Pres),
                NewMapAcc = maps:put(To, {NewPres, KPosts}, MapAcc),
                case sets:size(NewPres) of
                    0 ->
                        {[To | JobsAcc], NewMapAcc};
                    _ ->
                        {JobsAcc, NewMapAcc}
                end
        end,
    sets:fold(F, {Jobs, Map}, Posts).
