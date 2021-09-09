-module(aoc_2016_4_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    count(X, [], 0).

count([$- | T], Counts, 0) ->
    count(T, Counts, 0);
count([N | T], Counts, ID) when N >= $0, N =< $9 ->
    count(T, Counts, ID * 10 + (N - $0));
count([$[ | T], Counts, ID) ->
    hash(T, sort(Counts), ID);
count([X | T], Counts, ID) ->
    case lists:keyfind(X, 1, Counts) of
        false ->
            count(T, [{X, 1} | Counts], ID);
        {X, N} ->
            count(T, lists:keyreplace(X, 1, Counts, {X, N + 1}), ID)
    end.

hash([$]], _Counts, ID) ->
    ID;
hash([X | TL], [{X, _} | TC], ID) ->
    hash(TL, TC, ID);
hash(_, _, _) ->
    0.

sort(Counts) ->
    F = fun({X, N}, {Y, N}) ->
                X =< Y;
           ({_X, N}, {_Y, M}) ->
                N > M
        end,
    lists:sort(F, Counts).
