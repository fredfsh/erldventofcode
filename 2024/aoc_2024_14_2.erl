-module(aoc_2024_14_2).

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
    case io:fread("", "p=~d,~d v=~d,~d") of
        eof ->
            eof;
        {ok, [PX, PY, VX, VY]} ->
            {PX, PY, VX, VY}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    seconds(0, X).

seconds(Acc, X) ->
    case tree(Acc, X) of
        true ->
%%            print_tree(Acc, X),
            Acc;
        false ->
            seconds(Acc + 1, X)
    end.

-define(COLS, 101).
-define(ROWS, 103).

%% print_tree(Seconds, Robots) ->
%%     L = [pos(PX, PY, VX, VY, Seconds) || {PX, PY, VX, VY} <- Robots],
%%     Set = sets:from_list(L),
%%     F = fun(Y) ->
%%                 G = fun(X) ->
%%                             case sets:is_element({X, Y}, Set) of
%%                                 true ->
%%                                     io:format("#");
%%                                 false ->
%%                                     io:format(" ")
%%                             end
%%                     end,
%%                 lists:foreach(G, lists:seq(0, ?COLS - 1)),
%%                 io:format("~n")
%%         end,
%%     lists:foreach(F, lists:seq(0, ?ROWS - 1)).

tree(Seconds, Robots) ->
    %% The problem has it that MOST robots form a tree, not all of them.
    %% And images on Reddit indicate the tree may not be placed in the middle :(
    L = [pos(PX, PY, VX, VY, Seconds) || {PX, PY, VX, VY} <- Robots],
    Set = sets:from_list(L),
    F = fun(X) -> most_symmetric(Set, X) end,
    lists:any(F, lists:seq(1, ?COLS - 2)).

pos(PX, PY, VX, VY, Seconds) ->
    X = case (PX + VX * Seconds) rem ?COLS of
            N when N < 0 ->
                N + ?COLS;
            N ->
                N
        end,
    Y = case (PY + VY * Seconds) rem ?ROWS of
            M when M < 0 ->
                M + ?ROWS;
            M ->
                M
        end,
    {X, Y}.

-define(MOST, 0.5).

most_symmetric(Set, X) ->
    F = fun({PX, PY}) -> sets:is_element({X * 2 - PX, PY}, Set) end,
    Symmetric = sets:filter(F, Set),
    sets:size(Symmetric) / sets:size(Set) >= ?MOST.
