-module(aoc_2015_14_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl([]).

-define(FORMAT,
        "~s can fly ~d km/s for ~d seconds, but then must rest for ~d seconds."
       ).

input_impl(L) ->
    case io:fread("", ?FORMAT) of
        eof ->
            L;
        {ok, [_, Speed, Fly, Rest]} ->
            input_impl([{Speed, Fly, Rest} | L])
    end.

do(L) ->
    F = fun({Speed, Fly, Rest}, Acc) ->
                max(Acc, do_impl(Speed, Fly, Rest))
        end,
    lists:foldl(F, 0, L).

-define(TIME, 2503).

do_impl(Speed, Fly, Rest) ->
    T1 = ?TIME div (Fly + Rest) * Fly,
    T2 = min(?TIME rem (Fly + Rest), Fly),
    (T1 + T2) * Speed.
