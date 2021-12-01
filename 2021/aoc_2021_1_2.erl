-module(aoc_2021_1_2).

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
    Sums = sums(lists:reverse(X)),
    F = fun(Depth, {Acc, Last}) when Depth > Last ->
                {Acc + 1, Depth};
           (Depth, {Acc, _}) ->
                {Acc, Depth}
        end,
    {Res, _} = lists:foldl(F, {0, undefined}, Sums),
    Res.

sums(L) ->
    sums_impl([], L).

sums_impl(Acc, [_, _]) ->
    lists:reverse(Acc);
sums_impl(Acc, [A, B, C | T]) ->
    sums_impl([A + B + C | Acc], [B, C | T]).
