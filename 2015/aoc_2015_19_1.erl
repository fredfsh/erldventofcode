-module(aoc_2015_19_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl(maps:new()).

input_impl(M) ->
    {ok, [Seq]} = io:fread("", "~s"),
    case io:fread("", "~s ~s") of
        eof ->
            {M, val(Seq)};
        {ok, ["=>", Vs]} ->
            K = key(Seq),
            V = val(Vs),
            input_impl(add_mapping(K, V, M))
    end.

key([X]) when $a =< X, X =< $z ->
    undefined;
key([X]) when $A =< X, X =< $Z ->
    X - $A;
key([X, Y]) ->
    (X - $A) * 26 + (Y - $a).

val(Seq) ->
    val_impl(Seq, []).

val_impl([], Acc) ->
    lists:reverse(Acc);
val_impl([X, Y | T], Acc) when $a =< Y, Y =< $z ->
    val_impl(T, [key([X, Y]) | Acc]);
val_impl([X | T], Acc) ->
    val_impl(T, [key([X]) | Acc]).

add_mapping(K, V, M) ->
    L = maps:get(K, M, []),
    maps:put(K, [V | L], M).

do({M, Seq}) ->
    do_impl([], Seq, sets:new(), M).

do_impl(_RevPrefix, [], Set, _M) ->
    sets:size(Set);
do_impl(RevPrefix, [X | T], Set, M) ->
    L = maps:get(X, M, []),
    Set2 = replace(RevPrefix, T, L, Set),
    do_impl([X | RevPrefix], T, Set2, M).

replace(_RevPrefix, _Suffix, [], Set) ->
    Set;
replace(RevPrefix, Suffix, [X | T], Set) ->
    Y = lists:append([lists:reverse(RevPrefix), X, Suffix]),
    replace(RevPrefix, Suffix, T, sets:add_element(Y, Set)).
