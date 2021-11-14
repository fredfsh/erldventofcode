-module(aoc_2018_8_2).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    {Res, []} = input_node(lists:reverse(X)),
    Res.

input_node([Children, Metas | T]) ->
    {Subs, Rest} = input_children(T, Children),
    input_metas(Subs, Rest, Metas).

input_children(L, Children) ->
    F = fun(I, {MapAcc, LAcc}) ->
                {Val, Rest} = input_node(LAcc),
                {maps:put(I, Val, MapAcc), Rest}
        end,
    lists:foldl(F, {maps:new(), L}, lists:seq(1, Children)).

input_metas(Children, L, Metas) ->
    F = case maps:size(Children) of
            0 ->
                fun(_, {Acc, [H | T]}) ->
                        {Acc + H, T}
                end;
            _ ->
                fun(_, {Acc, [H | T]}) ->
                            {Acc + maps:get(H, Children, 0), T}
                end
        end,
    lists:foldl(F, {0, L}, lists:seq(1, Metas)).
