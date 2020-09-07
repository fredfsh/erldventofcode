-module(aoc_2015_19_2).

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

key([$e]) ->
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
    do_impl(1, length(Seq), undefined, Seq, M).

-define(TAB, memo).

do_impl(Start, End, Key, Seq, M) ->
    ets:new(?TAB, [named_table]),
    dp(Start, End, Key, Seq, M).

-define(IMPOSSIBLE, -1).

dp(Start, Start, Key, Seq, _M) ->
    case lists:nth(Start, Seq) of
        Key ->
            0;
        _ ->
            ?IMPOSSIBLE
    end;
dp(Start, End, Key, Seq, M) ->
    case ets:lookup(?TAB, {Start, End, Key}) of
        [] ->
            X = memo(Start, End, Key, Seq, M),
            ets:insert(?TAB, {{Start, End, Key}, X}),
            X;
        [{_K, Y}] ->
            Y
    end.

memo(Start, End, Key, Seq, M) ->
    Ls = maps:get(Key, M, []),
    F = fun(Val, Acc) ->
                N = replace(Start, End, Val, Seq, M),
                mini(Acc, inc(N))
        end,
    lists:foldl(F, ?IMPOSSIBLE, Ls).

replace(Start, End, Val, _Seq, _M)
  when End - Start + 1 < length(Val) ->
    ?IMPOSSIBLE;
replace(Start, End, Val, Seq, M) ->
    replace_impl(?IMPOSSIBLE, ?IMPOSSIBLE, Start, End - length(Val) + 1, Val,
                 Start, End, Seq, M).

replace_impl(_Acc, Min, I, E, _Val, _Start, _End, _Seq, _M)
  when I =:= E + 1 ->
    Min;
replace_impl(Acc, Min, _I, _E, [X], Start, End, Seq, M) ->
    N = dp(Start, End, X, Seq, M),
    mini(Min, accu(Acc, N));
replace_impl(Acc, Min, I, E, [X | T], Start, End, Seq, M) ->
    NewMin =
        case dp(Start, I, X, Seq, M) of
            ?IMPOSSIBLE ->
                Min;
            N ->
                NewAcc = accu(Acc, N),
                replace_impl(NewAcc, Min, I + 1, E + 1, T, I + 1, End, Seq, M)
        end,
    replace_impl(Acc, NewMin, I + 1, E, [X | T], Start, End, Seq, M).

inc(?IMPOSSIBLE) ->
    ?IMPOSSIBLE;
inc(X) ->
    X + 1.

accu(?IMPOSSIBLE, X) ->
    X;
accu(_Acc, ?IMPOSSIBLE) ->
    ?IMPOSSIBLE;
accu(Acc, X) ->
    Acc + X.

mini(X, ?IMPOSSIBLE) ->
    X;
mini(?IMPOSSIBLE, X) ->
    X;
mini(X, Y) ->
    min(X, Y).
