-module(aoc_2016_17_1).

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
    enqueue([{1, 1, [], X}], queue:new()).

-define(N, 4).

enqueue(L, Q) ->
    F = fun(_, PathAcc) when is_list(PathAcc) ->
                PathAcc;
           ({?N, ?N, Path, _Full}, _QAcc) ->
                Path;
           (X, QAcc) ->
                queue:in(X, QAcc)
        end,
    case lists:foldl(F, Q, L) of
        Path when is_list(Path) ->
            Path;
        Q2 ->
            bfs(Q2)
    end.

bfs(Q) ->
    {{value, {X, Y, Path, Full}}, Q2} = queue:out(Q),
    Candidates = next(X, Y, Path, Full),
    enqueue(Candidates, Q2).

next(X, Y, Path, Full) ->
    <<U:4, D:4, L:4, R:4, _/bitstring>> = crypto:hash(md5, Full),
    F = fun({Ch, XX, YY, Dir}, Acc)
              when Ch >= 11, XX > 0, XX =< ?N, YY > 0, YY =< ?N ->
                [{XX, YY,
                  lists:append(Path, [Dir]), lists:append(Full, [Dir])} | Acc];
           (_, Acc) ->
                Acc
        end,
    lists:foldl(F, [], [{U, X, Y - 1, $U},
                        {D, X, Y + 1, $D},
                        {L, X - 1, Y, $L},
                        {R, X + 1, Y, $R}]).
