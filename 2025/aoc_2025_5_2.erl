-module(aoc_2025_5_2).

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
    case io:get_line("") of
        eof ->
            eof;
        Line ->
            %%io:format("line: ~w~n", [Line]),
            case string:split(string:trim(Line), "-") of
                [_] ->
                    undefined;
                [From, To] ->
                    {list_to_integer(From), list_to_integer(To)}
            end
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, undefined) ->
    Acc;
acc(Acc, {From, To}) ->
    [{From, left}, {To, right} | Acc].

fin(X) ->
    fin_impl(0, undefined, lists:sort(X)).

fin_impl(Acc, undefined, []) ->
    Acc;
fin_impl(Acc, undefined, [{X, left} | T]) ->
    fin_impl(Acc, {X, 1}, T);
fin_impl(Acc, {Left, 1}, [{X, right} | T]) ->
    fin_impl(Acc + X - Left + 1, undefined, T);
fin_impl(Acc, {Left, Depth}, [{_, left} | T]) ->
    fin_impl(Acc, {Left, Depth + 1}, T);
fin_impl(Acc, {Left, Depth}, [{_, right} | T]) ->
    fin_impl(Acc, {Left, Depth - 1}, T).
