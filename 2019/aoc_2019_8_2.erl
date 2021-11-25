-module(aoc_2019_8_2).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    0.

-define(W, 25).
-define(H, 6).

do(L) ->
    F = fun(C, {Map, X, Y}) ->
                NewMap = case maps:is_key({X, Y}, Map) of
                             false when C =/= $2 ->
                                 maps:put({X, Y}, C, Map);
                             _ ->
                                 Map
                         end,
                {NX, NY} = case X + 1 of
                               ?W ->
                                   {0, (Y + 1) rem ?H};
                               _ ->
                                   {X + 1, Y}
                           end,
                {NewMap, NX, NY}
        end,
    {Image, _, _} = lists:foldl(F, {maps:new(), 0, 0}, L),
    print(Image),
    0.

print(Image) ->
    F = fun(Y) ->
                L = [ch(maps:get({X, Y}, Image)) || X <- lists:seq(0, ?W - 1)],
                io:format("~s~n", [L])
        end,
    lists:foreach(F, lists:seq(0, ?H - 1)),
    io:format("~n"),
    G = fun(Y) ->
                L = [ch($0 + $1 - maps:get({X, Y}, Image))
                     || X <- lists:seq(0, ?W - 1)],
                io:format("~s~n", [L])
        end,
    lists:foreach(G, lists:seq(0, ?H - 1)).

-define(SPACE, 16#20).

ch($0) ->
    $.;
ch($1) ->
    ?SPACE.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
