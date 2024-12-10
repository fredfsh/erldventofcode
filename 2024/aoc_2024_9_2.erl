-module(aoc_2024_9_2).

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
    {Data, Free} = input_data({[], array:new([10, {default, []}])}, L, 0, 0),
    {Data, array:map(fun(_, X) -> lists:reverse([infinity | X]) end, Free)}.

input_data(Acc, [], _, _) ->
    Acc;
input_data({Data, Free}, [H | T], FD, BID) ->
    N = H - $0,
    input_free({[{BID, FD, N} | Data], Free}, T, FD + 1, BID + N).

input_free(Acc, [], _, _) ->
    Acc;
input_free({Data, Free}, [H | T], FD, BID) ->
    N = H - $0,
    L = array:get(N, Free),
    input_data({Data, array:set(N, [BID | L], Free)}, T, FD, BID + N).

ini() ->
    0.

do({Data, Free}) ->
    checksum(defrag(Data, Free)).

defrag(Data, Free) ->
    defrag_impl([], Data, Free).

defrag_impl(Acc, [], _) ->
%%    io:format("~p~n~p~n", [Acc, array:to_list(Free)]),
    Acc;
defrag_impl(Acc, [{BID, FD, N} = File | T], Free) ->
    case victim(N, Free) of
        {infinity, _} ->
            defrag_impl([File | Acc], T, Free);
        {BlockID, _} when BlockID > BID ->
            defrag_impl([File | Acc], T, Free);
        {BlockID, NB} ->
            NAcc = [{BlockID, FD, N} | Acc],
            Free2 = array:set(NB, tl(array:get(NB, Free)), Free),
            NFree = case NB of
                        N ->
                            Free2;
                        _ ->
                            insert_free(BlockID + N, NB - N, Free2)
                    end,
            defrag_impl(NAcc, T, NFree)
    end.

victim(N, Free) ->
    L = [{hd(array:get(I, Free)), I} || I <- lists:seq(N, 9)],
    hd(lists:sort(L)).

insert_free(BID, N, Free) ->
    L = array:get(N, Free),
    array:set(N, lists:sort([BID | L]), Free).

checksum(Data) ->
    L = [FD * (BID + BID + N - 1) * N div 2 || {BID, FD, N} <- Data],
    lists:sum(L).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
