-module(aoc_2015_14_2).

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

-define(TIME, 2503).

do(L) ->
    do_impl(1, [0 || _ <- L], [0 || _ <- L], L).

do_impl(Time, _Dists, Pts, _L) when Time > ?TIME ->
    lists:max(Pts);
do_impl(Time, Dists, Pts, L) ->
    F = fun({Speed, Fly, Rest}, {DistsAcc, MaxAcc}) ->
                [X | T] = DistsAcc,
                Dist = case Time rem (Fly + Rest) of
                           N when N > 0, N =< Fly ->
                               X + Speed;
                           _ ->
                               X
                       end,
                {Dist, {T, max(MaxAcc, Dist)}}
        end,
    {NewDists, {[], MaxDist}} = lists:mapfoldl(F, {Dists, 0}, L),

    G = fun(Pt, [X | T]) when X =:= MaxDist ->
                {Pt + 1, T};
           (Pt, [_ | T]) ->
                {Pt, T}
        end,
    {NewPts, []} = lists:mapfoldl(G, NewDists, Pts),

    do_impl(Time + 1, NewDists, NewPts, L).
