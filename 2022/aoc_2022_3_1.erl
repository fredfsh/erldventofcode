-module(aoc_2022_3_1).

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
    case io:get_line("") of
        eof ->
            eof;
        L ->
            string:trim(L)
    end.

ini() ->
    0.

do(X) ->
    L = length(X),
    do_impl(1, X, sets:new(), L).

do_impl(I, [H | T], Set, L) when I =< L / 2->
    do_impl(I + 1, T, sets:add_element(H, Set), L);
do_impl(I, [H | T], Set, L) ->
    case sets:is_element(H, Set) of
        false ->
            do_impl(I + 1, T, Set, L);
        true ->
            case H =< $z andalso H >= $a of
                true ->
                    H - $a + 1;
                false ->
                    H - $A + 27
            end
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
