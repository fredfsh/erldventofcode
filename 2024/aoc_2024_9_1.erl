-module(aoc_2024_9_1).

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
            input_blocks(X)
    end.

input_blocks(L) ->
    input_data(queue:new(), L, 0).

input_data(Q, [], _) ->
    Q;
input_data(Q, [H | T], ID) ->
    input_free(queue:in({ID, H - $0}, Q), T, ID + 1).

input_free(Q, [], _) ->
    Q;
input_free(Q, [H | T], ID) ->
    input_data(queue:in(H - $0, Q), T, ID).

ini() ->
    0.

do(Q) ->
    checksum(Q).

checksum(Q) ->
    checksum_impl(0, 0, Q).

checksum_impl(Acc, BID, Q) ->
%%    io:format("queue: ~p~n", [queue:to_list(Q)]),
    case queue:out(Q) of
        {empty, _} ->
            Acc;
        {{value, {FD, NB}}, Q2} ->
            {NBID, Checksum} = calc_checksum(BID, FD, NB),
%%            io:format("checksum({~p, ~p}) = ~p~n~n", [FD, NB, Checksum]),
            checksum_impl(Acc + Checksum, NBID, Q2);
        {{value, NB}, Q2} ->
            NQ = fill_free(NB, Q2),
%%            io:format("queue: ~p~n~n", [queue:to_list(NQ)]),
            checksum_impl(Acc, BID, NQ)
    end.

calc_checksum(BID, FD, N) ->
    calc_checksum_impl(0, BID, N, FD).

calc_checksum_impl(Acc, BID, 0, _) ->
    {BID, Acc};
calc_checksum_impl(Acc, BID, N, FD) ->
    calc_checksum_impl(Acc + BID * FD, BID + 1, N - 1, FD).

fill_free(N, Q) ->
    case queue:out_r(Q) of
        {empty, _} ->
            Q;
        {{value, {FD, NB}}, Q2} when NB =:= N ->
            queue:in_r({FD, N}, Q2);
        {{value, {FD, NB}}, Q2} when NB > N ->
            queue:in({FD, NB - N}, queue:in_r({FD, N}, Q2));
        {{value, {FD, NB}}, Q2} when NB < N ->
            queue:in_r({FD, NB}, queue:in_r(N - NB, Q2));
        {{value, _}, Q2} ->
            queue:in_r(N, Q2)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
