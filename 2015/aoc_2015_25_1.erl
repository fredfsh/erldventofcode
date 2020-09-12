-module(aoc_2015_25_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

-define(FMT,
        "To continue, please consult the code grid in the manual.  "
        "Enter the code at row ~d, column ~d.").

input() ->
    {ok, [Row, Col]} = io:fread("", ?FMT),
    {Row, Col}.

-define(START, 27995004).
-define(MUL, 252533).
-define(MOD, 33554393).

do({Row, Col}) ->
    X = nth(6, 6),
    Y = nth(Row, Col),
    do_impl(?START, Y - X, ?MUL).

nth(Row, Col) ->
    Diag = Row + Col - 1,
    Diag * (Diag - 1) bsr 1 + Col.

do_impl(X, 0, _Mul) ->
    X;
do_impl(X, N, Mul) when N band 1 =:= 0 ->
    do_impl(X, N bsr 1, Mul * Mul rem ?MOD);
do_impl(X, N, Mul) ->
    do_impl(X * Mul rem ?MOD, N bsr 1, Mul * Mul rem ?MOD).
