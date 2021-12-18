-module(aoc_2021_18_1).

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
            F = fun(N) when N >= $0, N =< $9 ->
                        {n, N - $0};
                   (C) ->
                        C
                end,
            lists:map(F, X)
    end.

ini() ->
    undefined.

do(X) ->
    X.

acc(undefined, X) ->
    X;
acc(X, Y) ->
    add(X, Y).

add(X, Y) ->
    maybe_reduce(lists:append([[$[], X, [$,], Y, [$]]])).

maybe_reduce(L) ->
    case maybe_explode(L) of
        false ->
            case maybe_split(L) of
                false ->
                    L;
                L2 ->
                    maybe_reduce(L2)
            end;
        L2 ->
            maybe_reduce(L2)
    end.

maybe_explode(L) ->
    maybe_explode_impl(0, L, []).

-define(DEPTH, 4).

maybe_explode_impl(_, [], _) ->
    false;
maybe_explode_impl(Depth, [$[, {n, L}, $,, {n, R}, $] | T], Past)
  when Depth >= ?DEPTH ->
    RevLeft = inc(Past, L),
    Right = inc(T, R),
    lists:append([lists:reverse(RevLeft), [{n, 0}], Right]);
maybe_explode_impl(Depth, [$[ | T], Past) ->
    maybe_explode_impl(Depth + 1, T, [$[ | Past]);
maybe_explode_impl(Depth, [$] | T], Past) ->
    maybe_explode_impl(Depth - 1, T, [$] | Past]);
maybe_explode_impl(Depth, [H | T], Past) ->
    maybe_explode_impl(Depth, T, [H | Past]).

inc(L, X) ->
    inc_impl([], L, X).

inc_impl(Past, [], _) ->
    lists:reverse(Past);
inc_impl(Past, [{n, N} | T], X) ->
    lists:append(lists:reverse(Past), [{n, N + X} | T]);
inc_impl(Past, [H | T], X) ->
    inc_impl([H | Past], T, X).

maybe_split(L) ->
    maybe_split_impl([], L).

maybe_split_impl(_, []) ->
    false;
maybe_split_impl(Past, [{n, N} | T]) when N >= 10 ->
    L = N div 2,
    R = N - L,
    lists:append(lists:reverse(Past), [$[, {n, L}, $,, {n, R}, $] | T]);
maybe_split_impl(Past, [H | T]) ->
    maybe_split_impl([H | Past], T).

fin(X) ->
    {Tree, []} = input_tree(X),
    mag(Tree).

-record(tree, {l, r}).

input_tree([$[ | T]) ->
    {Left, [$, | Rest]} = input_node(T),
    {Right, [$] | Rest2]} = input_node(Rest),
    {#tree{l = Left, r = Right}, Rest2}.

input_node([$[ | _] = L) ->
    input_tree(L);
input_node([{n, N} | T]) ->
    {N, T}.

mag(#tree{l = L, r = R}) ->
    3 * mag(L) + 2 * mag(R);
mag(X) ->
    X.
