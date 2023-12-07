-module(aoc_2023_7_2).

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
    case io:fread("", "~s ~d") of
        eof ->
            eof;
        {ok, [Hand, Bid]} ->
            {Hand, Bid}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(L) ->
    F = fun({HA, _}, {HB, _}) ->
                case {max_type(HA), max_type(HB)} of
                    {X, X} ->
                        compare(HA, HB);
                    {X, Y} ->
                        X < Y
                end
        end,
    Sorted = lists:sort(F, L),
    Indexed = lists:enumerate(Sorted),
    lists:sum([I * Bid || {I, {_, Bid}} <- Indexed]).

-define(LABELS, "AKQT98765432").

max_type(Hand) ->
    F = fun(Label, Acc) ->
                Replaced = lists:flatten(string:replace(Hand, "J", [Label], all)),
                max(Acc, type(Replaced))
        end,
    lists:foldl(F, 0, ?LABELS).

type(Hand) ->
    type_impl(lists:sort(Hand)).

type_impl([X, X, X, X, X]) ->
    7;
type_impl([X, X, X, X, _]) ->
    6;
type_impl([_, X, X, X, X]) ->
    6;
type_impl([X, X, X, Y, Y]) ->
    5;
type_impl([X, X, Y, Y, Y]) ->
    5;
type_impl([X, X, X, _, _]) ->
    4;
type_impl([_, X, X, X, _]) ->
    4;
type_impl([_, _, X, X, X]) ->
    4;
type_impl([X, X, Y, Y, _]) ->
    3;
type_impl([X, X, _, Y, Y]) ->
    3;
type_impl([_, X, X, Y, Y]) ->
    3;
type_impl([X, X, _, _, _]) ->
    2;
type_impl([_, X, X, _, _]) ->
    2;
type_impl([_, _, X, X, _]) ->
    2;
type_impl([_, _, _, X, X]) ->
    2;
type_impl(_) ->
    1.

compare([HA | TA], [HB | TB]) ->
    case {label(HA), label(HB)} of
        {X, X} ->
            compare(TA, TB);
        {X, Y} ->
            X < Y
    end.

label($A) ->
    ace;
label($K) ->
    13;
label($Q) ->
    12;
label($J) ->
    0;
label($T) ->
    10;
label(X) ->
    X - $0.
