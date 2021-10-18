-module(aoc_2016_15_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl([]).

input_impl(Acc) ->
    Format = "Disc #~d has ~d positions; at time=0, it is at position ~d.",
    case io:fread("", Format) of
        eof ->
            Acc;
        {ok, [I, N, P]} ->
            input_impl([{I, P, N} | Acc])
    end.

do(X) ->
    do_impl(0, 1, X).

do_impl(X, _LCD, []) ->
    X;
do_impl(X, LCD, [{I, P, N} | T]) ->
    Y = inc(X, LCD, I, P, N),
    NewLCD = lcd(LCD, N),
    do_impl(Y, NewLCD, T).

inc(X, _LCD, I, P, N) when (X + I + P) rem N =:= 0 ->
    X;
inc(X, LCD, I, P, N) ->
    inc(X + LCD, LCD, I, P, N).

lcd(X, Y) ->
    X * Y div gcd(X, Y).

gcd(X, Y) when X < Y ->
    gcd(Y, X);
gcd(X, Y) when X rem Y =:= 0 ->
    Y;
gcd(X, Y) ->
    gcd(Y, X rem Y).
