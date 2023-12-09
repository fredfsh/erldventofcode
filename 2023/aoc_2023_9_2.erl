-module(aoc_2023_9_2).

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
        L ->
            NStrs = string:split(string:trim(L), " ", all),
            [list_to_integer(X) || X <- NStrs]
    end.

ini() ->
    0.

do(X) ->
    Ls = reduce(X),
    extrapolate(Ls).

reduce(L) ->
    reduce_impl([L], L).

reduce_impl(Acc, L) ->
    case diff(L) of
        true ->
            Acc;
        Diffs ->
            reduce_impl([Diffs | Acc], Diffs)
    end.

diff(L) ->
    diff_impl([], true, L).

diff_impl(Acc, Zeroes, [_]) ->
    Zeroes orelse lists:reverse(Acc);
diff_impl(Acc, Zeroes, [A, A | T]) ->
    diff_impl([0 | Acc], Zeroes, [A | T]);
diff_impl(Acc, _, [A, B | T]) ->
    diff_impl([B - A | Acc], false, [B | T]).

extrapolate(Ls) ->
    Tail = hd(hd(Ls)),
    extrapolate_impl(Tail, tl(Ls)).

extrapolate_impl(Acc, []) ->
    Acc;
extrapolate_impl(Acc, [H | T]) ->
    extrapolate_impl(hd(H) - Acc, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
