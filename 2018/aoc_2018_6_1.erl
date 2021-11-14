-module(aoc_2018_6_1).

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
    case io:fread("", "~d, ~d") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    {0, []}.

do(X) ->
    X.

acc({I, L}, {X, Y}) ->
    {I + 1, [{I, X, Y} | L]}.

fin({_, Inits}) ->
    {MinX, MinY, MaxX, MaxY} = borders(Inits),
    L = [{X, Y, val(X, Y, Inits)} || X <- lists:seq(MinX, MaxX),
                                     Y <- lists:seq(MinY, MaxY)],
    largest(L, MinX, MinY, MaxX, MaxY).

borders(L) ->
    F = fun({_, X, Y}, {MinX, MinY, MaxX, MaxY}) ->
                {min(X, MinX), min(Y, MinY), max(X, MaxX), max(Y, MaxY)}
        end,
    lists:foldl(F, {undefined, undefined, -1, -1}, L).

val(X, Y, Inits) ->
    F = fun({I, XX, YY}, {Dist, ID}) ->
                case abs(X - XX) + abs(Y - YY) of
                    D when D < Dist ->
                        {D, I};
                    Dist ->
                        {Dist, -1};
                    _ ->
                        {Dist, ID}
                end
        end,
    {_, ID} = lists:foldl(F, {undefined, undefined}, Inits),
    ID.

largest(L, MinX, MinY, MaxX, MaxY) ->
    F = fun({_, _, -1}, Acc) ->
                Acc;
           ({X, Y, ID}, {MapAcc, SetAcc}) when X =:= MinX; X =:= MaxX;
                                               Y =:= MinY; Y =:= MaxY ->
                {MapAcc, sets:add_element(ID, SetAcc)};
           ({_, _, ID}, {MapAcc, SetAcc}) ->
                {maps:update_with(ID, fun(V) -> V + 1 end, 1, MapAcc), SetAcc}
        end,
    {Counts, Infis} = lists:foldl(F, {maps:new(), sets:new()}, L),
    Finites = maps:filter(fun(K,_) -> not sets:is_element(K, Infis) end, Counts),
    {_, Count} = lists:last(lists:keysort(2, maps:to_list(Finites))),
    Count.
