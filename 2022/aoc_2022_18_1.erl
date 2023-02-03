-module(aoc_2022_18_1).

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
    case io:fread("", "~d,~d,~d") of
        eof ->
            eof;
        {ok, [X, Y, Z]} ->
            {X, Y, Z}
    end.

ini() ->
    sets:new().

do(X) ->
    X.

acc(Acc, X) ->
    sets:add_element(X, Acc).

-define(D, [{-1,  0,  0},
            { 1,  0,  0},
            { 0, -1,  0},
            { 0,  1,  0},
            { 0,  0, -1},
            { 0,  0,  1}]).

fin(Set) ->
    F = fun({X, Y, Z}, Acc) ->
                G = fun({DX, DY, DZ}, GAcc) ->
                            case sets:is_element({X+DX, Y+DY, Z+DZ}, Set) of
                                true ->
                                    GAcc;
                                false ->
                                    GAcc + 1
                            end
                    end,
                lists:foldl(G, Acc, ?D)
        end,
    sets:fold(F, 0, Set).
