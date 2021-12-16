-module(aoc_2021_16_1).

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
    versions(Bin).

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

versions(Bin) ->
    versions_impl(0, Bin).

versions_impl(Acc, Bin) ->
    case version(Bin) of
        {V, Rest} ->
            versions_impl(Acc + V, Rest);
        _ ->
            Acc
    end.

version(<<V:3, 4:3, T/bitstring>>) ->
    Rest = literal(T),
    {V, Rest};
version(<<V:3, _:3, 0:1, Len:15, Bin:Len/bitstring, T/bitstring>>) ->
    {V + versions(Bin), T};
version(<<V:3, _:3, 1:1, Packets:11, T/bitstring>>) ->
    F = fun(_, {Acc, BinAcc}) ->
                {Version, Rest} = version(BinAcc),
                {Acc + Version, Rest}
        end,
    lists:foldl(F, {V, T}, lists:seq(1, Packets));
version(_) ->
    undefined.

literal(<<1:1, _:4, T/bitstring>>) ->
    literal(T);
literal(<<0:1, _:4, T/bitstring>>) ->
    T.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
