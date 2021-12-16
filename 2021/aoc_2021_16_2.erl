-module(aoc_2021_16_2).

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
            X
    end.

ini() ->
    0.

do(X) ->
    Bin = bin(X),
    {Res, _} = value(Bin),
    Res.

bin(X) ->
    bin_impl(<<>>, X).
bin(H1, H2) ->
    V = (val(H1) bsl 4) + val(H2),
    <<V>>.

bin_impl(Acc, []) ->
    Acc;
bin_impl(Acc, [H]) ->
    Bin = bin(H, 0),
    <<Acc/binary, Bin/binary>>;
bin_impl(Acc, [H1, H2 | T]) ->
    Bin = bin(H1, H2),
    bin_impl(<<Acc/binary, Bin/binary>>, T).

val(X) when X >= $0, X =< $9 ->
    X - $0;
val(X) ->
    X - $A + 10.

value(<<_:3, 4:3, T/bitstring>>) ->
    literal(T);
value(<<_:3, Type:3, 0:1, Len:15, Bin:Len/bitstring, T/bitstring>>) ->
    Values = values(Bin),
    {calc(Type, Values), T};
value(<<_:3, Type:3, 1:1, Packets:11, T/bitstring>>) ->
    F = fun(_, {Acc, BinAcc}) ->
                {Value, Rest} = value(BinAcc),
                {[Value | Acc], Rest}
        end,
    {Values, Rest} = lists:foldl(F, {[], T}, lists:seq(1, Packets)),
    {calc(Type, lists:reverse(Values)), Rest}.

values(Bin) ->
    values_impl([], Bin).

values_impl(Acc, <<>>) ->
    lists:reverse(Acc);
values_impl(Acc, Bin) ->
    {Value, Rest} = value(Bin),
    values_impl([Value | Acc], Rest).

calc(0, Values) ->
    lists:sum(Values);
calc(1, Values) ->
    F = fun(X, Acc) -> X * Acc end,
    lists:foldl(F, 1, Values);
calc(2, Values) ->
    lists:min(Values);
calc(3, Values) ->
    lists:max(Values);
calc(5, [V1, V2]) when V1 > V2 ->
    1;
calc(5, _) ->
    0;
calc(6, [V1, V2]) when V1 < V2 ->
    1;
calc(6, _) ->
    0;
calc(7, [V, V]) ->
    1;
calc(7, _) ->
    0.

literal(Bin) ->
    literal_impl(0, Bin).

literal_impl(Acc, <<1:1, V:4, T/bitstring>>) ->
    literal_impl((Acc bsl 4) + V, T);
literal_impl(Acc, <<0:1, V:4, T/bitstring>>) ->
    {(Acc bsl 4) + V, T}.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
