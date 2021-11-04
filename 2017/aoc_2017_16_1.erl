-module(aoc_2017_16_1).

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
            [input_move(P) || P <- string:split(X, ",", all)]
    end.

input_move([$s | T]) ->
    {s, list_to_integer(T)};
input_move([$x | T]) ->
    [A, B] = string:split(T, "/"),
    {x, list_to_integer(A), list_to_integer(B)};
input_move([$p | T]) ->
    [[A], [B]] = string:split(T, "/"),
    {p, A, B}.

ini() ->
    "".

-define(N, 16).

do(X) ->
    Init = [$a + I || I <- lists:seq(0, ?N - 1)],
    do_impl(Init, X).

do_impl(Acc, []) ->
    Acc;
do_impl(Acc, [{s, X} | T]) ->
    do_impl(spin(Acc, X), T);
do_impl(Acc, [{x, A, B} | T]) ->
    do_impl(exchange(Acc, A, B), T);
do_impl(Acc, [{p, A, B} | T]) ->
    do_impl(partner(Acc, A, B), T).

spin(S, X) ->
    {Left, Right} = lists:split(length(S) - X, S),
    lists:append(Right, Left).

exchange(S, A, A) ->
    S;
exchange(S, A, B) when A > B ->
    exchange(S, B, A);
exchange(S, A, B) ->
    lists:append([string:slice(S, 0, A),
                  string:slice(S, B, 1),
                  string:slice(S, A + 1, B - A - 1),
                  string:slice(S, A, 1),
                  string:slice(S, B + 1)]).

partner(S, A, A) ->
    S;
partner(S, A, B) ->
    exchange(S, index(A, S), index(B, S)).

index(X, S) ->
    index_impl(0, X, S).

index_impl(I, X, [X | _]) ->
    I;
index_impl(I, X, [_ | T]) ->
    index_impl(I + 1, X, T).

acc(Acc, X) ->
    Acc ++ X.

fin(X) ->
    X.
