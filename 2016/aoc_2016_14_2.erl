-module(aoc_2016_14_2).

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
    {Threes, Fives} = init(X),
    do_impl(0, undefined, Threes, Fives, X).

init(Salt) ->
    F = fun(I, {QAcc, MapAcc}) ->
                aggregate(I, QAcc, MapAcc, Salt)
        end,
    lists:foldl(F, {queue:new(), maps:new()}, lists:seq(0, 1000)).

aggregate(I, Q, Map, Salt) ->
    Hash = hash(I, Salt),
    {Ch, Fives} = aggregate_impl(Hash, Map, I),
    {queue:in({I, Ch}, Q), Fives}.

hash(X, Salt) ->
    hash_impl(0, crypto:hash(md5, lists:append(Salt, integer_to_list(X)))).

-define(HASHES, 2016).

hash_impl(?HASHES, MD5) ->
    MD5;
hash_impl(Count, MD5) ->
    hash_impl(Count + 1, crypto:hash(md5, hex(MD5))).

hex(X) when is_binary(X) ->
    hex_impl(X, []);
hex(X) when is_integer(X), X >= 0, X =< 9->
    X + $0;
hex(X) when is_integer(X) ->
    X - 10 + $a.

hex_impl(<<>>, L) ->
    lists:reverse(L);
hex_impl(<<H:4, T/bitstring>>, L) ->
    hex_impl(T, [hex(H) | L]).


aggregate_impl(MD5, Map, I) ->
    aggregate_impl(undefined, undefined, undefined, undefined, MD5, undefined, Map, I).

aggregate_impl(_, _, _, _, <<>>, Ch, Map, _I) ->
    {Ch, Map};
aggregate_impl(H, H, H, H, <<H:4, T/bitstring>>, Ch, Map, I) ->
    F = fun(L) -> [I | L] end,
    aggregate_impl(H, H, H, H, T, first(Ch, H),
                   maps:update_with(H, F, [I], Map), I);
aggregate_impl(_, L, H, H, <<H:4, T/bitstring>>, Ch, Map, I) ->
    aggregate_impl(L, H, H, H, T, first(Ch, H), Map, I);
aggregate_impl(_, L3, L2, L1, <<H:4, T/bitstring>>, Ch, Map, I) ->
    aggregate_impl(L3, L2, L1, H, T, Ch, Map, I).

first(undefined, H) ->
    H;
first(Ch, _) ->
    Ch.

-define(NTH, 64).

do_impl(?NTH, Index, _Threes, _Fives, _Salt) ->
    Index;
do_impl(Count, Index, Threes, Fives, Salt) ->
    {{value, {I, Ch}}, Tail} = queue:out(Threes),
    {Q, Map} = aggregate(I + 1001, Tail, Fives, Salt),
    case search(Ch, I, Fives) of
        true ->
            do_impl(Count + 1, I, Q, Map, Salt);
        false ->
            do_impl(Count, Index, Q, Map, Salt)
    end.

search(undefined, _, _) ->
    false;
search(Ch, I, Fives) ->
    L = maps:get(Ch, Fives, []),
    lists:any(fun(N) -> N >= I + 1 andalso N =< I + 1000 end, L).
