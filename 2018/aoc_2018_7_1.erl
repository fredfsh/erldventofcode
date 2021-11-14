-module(aoc_2018_7_1).

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

fin(Map) ->
    Init = maps:keys(maps:filter(fun(_, {P, _}) -> sets:size(P) =:= 0 end, Map)),
    sequence([], lists:sort(Init), Map).

sequence(Acc, [], _) ->
    lists:reverse(Acc);
sequence(Acc, [H | T], Map) ->
    {_, Posts} = maps:get(H, Map),
    F = fun(To, {LAcc, MapAcc}) ->
                {Pres, KPosts} = maps:get(To, MapAcc),
                NewPres = sets:del_element(H, Pres),
                NewMapAcc = maps:put(To, {NewPres, KPosts}, MapAcc),
                case sets:size(NewPres) of
                    0 ->
                        {[To | LAcc], NewMapAcc};
                    _ ->
                        {LAcc, NewMapAcc}
                end
        end,
    {NewL, NewMap} = sets:fold(F, {T, Map}, Posts),
    sequence([H | Acc], lists:sort(NewL), NewMap).
