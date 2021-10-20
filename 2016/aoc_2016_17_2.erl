-module(aoc_2016_17_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    {ok, [X]} = io:fread("", "~s"),
    X.

do(X) ->
    dfs(0, 1, 1, X).

-define(N, 4).

dfs(Length, ?N, ?N, _Path) ->
    Length;
dfs(Length, X, Y, Path) ->
    Candidates = next(X, Y, Path),
    F = fun({XX, YY, PP}, Longest) ->
                case dfs(Length + 1, XX, YY, PP) of
                    N when N > Longest ->
                        N;
                    _ ->
                        Longest
                end
        end,
    lists:foldl(F, -1, Candidates).

next(X, Y, Path) ->
    <<U:4, D:4, L:4, R:4, _/bitstring>> = crypto:hash(md5, Path),
    F = fun({Ch, XX, YY, Dir}, Acc)
              when Ch >= 11, XX > 0, XX =< ?N, YY > 0, YY =< ?N ->
                [{XX, YY, lists:append(Path, [Dir])} | Acc];
           (_, Acc) ->
                Acc
        end,
    lists:foldl(F, [], [{U, X, Y - 1, $U},
                        {D, X, Y + 1, $D},
                        {L, X - 1, Y, $L},
                        {R, X + 1, Y, $R}]).
