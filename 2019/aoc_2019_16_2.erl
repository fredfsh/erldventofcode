-module(aoc_2019_16_2).

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
            [C - $0 || C <- X]
    end.

ini() ->
    "".

-define(PHASES, 100).
-define(REPEAT, 10000).

do(X) ->
    Skip = skip(X),
    N = length(X) * ?REPEAT - Skip,
    XX = suffix(N, X),
    G = fun(E, []) ->
                [E];
           (E, Acc) ->
                [(hd(Acc) + E) rem 10 | Acc]
        end,
    F = fun(_, Acc) ->
                lists:foldl(G, [], lists:reverse(Acc))
        end,
    L = lists:foldl(F, XX, lists:seq(1, ?PHASES)),
    [C + $0 || C <- lists:sublist(L, 8)].

-define(SKIPS, 7).

skip(X) ->
    list_to_integer([C + $0 || C <- lists:sublist(X, ?SKIPS)]).

suffix(N, X) ->
    R = lists:reverse(X),
    L = length(R),
    C = N div L,
    LL = lists:duplicate(C, R),
    lists:reverse(lists:flatten(lists:append(LL, [lists:sublist(R, N-L*C)]))).

acc(Acc, X) ->
    Acc ++ X.

fin(X) ->
    X.
