-module(aoc_2019_24_1).

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
            string:trim(X, trailing, "\n")
    end.

-define(N, 5).

ini() ->
    {1, sets:new()}.

do(X) ->
    X.

acc({Y, Bugs}, L) ->
    F = fun({_, $.}, Acc) ->
                Acc;
           ({X, $#}, Acc) ->
                sets:add_element({X, Y}, Acc)
        end,
    {Y + 1, lists:foldl(F, Bugs, lists:zip(lists:seq(1, ?N), L))}.

fin({_, Bugs}) ->
    game(Bugs, sets:new()).

game(Bugs, Seen) ->
    case sets:is_element(Bugs, Seen) of
        true ->
            rating(Bugs);
        false ->
            Next = evolve(Bugs),
            game(Next, sets:add_element(Bugs, Seen))
    end.

rating(Bugs) ->
    F = fun(I, {Sum, Cur}) ->
                Y = (I - 1) div ?N + 1,
                X = (I - 1) rem ?N + 1,
                case sets:is_element({X, Y}, Bugs) of
                    false ->
                        {Sum, Cur bsl 1};
                    true ->
                        {Sum + Cur, Cur bsl 1}
                end
        end,
    {Res, _} = lists:foldl(F, {0, 1}, lists:seq(1, ?N * ?N)),
    Res.

evolve(Bugs) ->
    F = fun(I, Acc) ->
                Y = (I - 1) div ?N + 1,
                X = (I - 1) rem ?N + 1,
                case count(X, Y, Bugs) of
                    {false, 1} ->
                        sets:add_element({X, Y}, Acc);
                    {false, 2} ->
                        sets:add_element({X, Y}, Acc);
                    {true, 1} ->
                        sets:add_element({X, Y}, Acc);
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, maps:new(), lists:seq(1, ?N * ?N)).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

count(X, Y, Bugs) ->
    L = [{X + DX, Y + DY} || {DX, DY} <- ?D],
    Count = length(lists:filter(fun(Z) -> sets:is_element(Z, Bugs) end, L)),
    {sets:is_element({X, Y}, Bugs), Count}.
