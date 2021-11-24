-module(aoc_2019_6_1).

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
            string:split(X, ")")
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, [Inner, Outer]) ->
    maps:put(Outer, Inner, Acc).

-define(TAB, memo).

fin(X) ->
    ets:new(?TAB, [named_table]),
    maps:foreach(fun(Outer, _) -> depth(Outer, X) end, X),
    lists:sum(ets:select(?TAB, [{{'_', '$1'}, [], ['$1']}])).

depth(Outer, Orbits) ->
    case ets:lookup(?TAB, Outer) of
        [] ->
            Res = case maps:get(Outer, Orbits, undefined) of
                      undefined ->
                          0;
                      Inner ->
                          depth(Inner, Orbits) + 1
                  end,
            ets:insert_new(?TAB, [{Outer, Res}]),
            Res;
        [{_, N}] ->
            N
    end.
