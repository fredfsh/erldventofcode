-module(aoc_2016_5_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    Prefix = input(),
    do(Prefix).

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    do_impl(maps:new(), 1, X).

-define(LEN, 8).

do_impl(Map, N, X) ->
    case maps:size(Map) of
        ?LEN ->
            password(Map);
        _ ->
            case crypto:hash(md5, X ++ integer_to_list(N)) of
                <<0, 0, 0:4, P:4, H:4, _:100>> when P < ?LEN ->
                    case maps:is_key(P, Map) of
                        false ->
                            do_impl(maps:put(P, hex(H), Map), N + 1, X);
                        _ ->
                            do_impl(Map, N + 1, X)
                    end;
                _ ->
                    do_impl(Map, N + 1, X)
            end
    end.

password(Map) ->
    [maps:get(X, Map) || X <- lists:seq(0, ?LEN - 1)].

hex(N) when N =< 9 ->
    N + $0;
hex(N) ->
    N - 10 + $a.
