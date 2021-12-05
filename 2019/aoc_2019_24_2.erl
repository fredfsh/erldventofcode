-module(aoc_2019_24_2).

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

-define(MINUTES, 200).

fin({_, Bugs}) ->
    B = (?MINUTES + 1) div 2,
    Layers = maps:from_list([{L, sets:new()} || L <- lists:seq(-B, B)]),
    game(maps:put(0, Bugs, Layers), ?MINUTES).

game(Layers, 0) ->
    maps:fold(fun(_, Set, Acc) -> Acc + sets:size(Set) end, 0, Layers);
game(Layers, Minutes) ->
    game(evolve(Layers), Minutes - 1).

evolve(Layers) ->
    F = fun(Layer, _) ->
                G = fun(13, Acc) ->
                            Acc;
                       (I, Acc) ->
                            Y = (I - 1) div ?N + 1,
                            X = (I - 1) rem ?N + 1,
                            case count(Layer, X, Y, Layers) of
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
                lists:foldl(G, sets:new(), lists:seq(1, ?N * ?N))
        end,
    maps:map(F, Layers).

count(Layer, X0, Y0, Layers) ->
    F = fun({L, X, Y}, Acc) ->
                case bug(L, X, Y, Layers) of
                    true ->
                        Acc + 1;
                    false ->
                        Acc
                end
        end,
    {bug(Layer, X0, Y0, Layers), lists:foldl(F, 0, neighbors(Layer, X0, Y0))}.

bug(L, X, Y, Layers) ->
    sets:is_element({X, Y}, maps:get(L, Layers, sets:new())).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

%% {2, 2}, {2, 4}, {4, 2}, {4, 4}
neighbors(L, X, Y) when X rem 2 =:= 0, Y rem 2 =:= 0 ->
    [{L, X + DX, Y + DY} || {DX, DY} <- ?D];

%% {1, X}
neighbors(L, 1, 1) ->
    [{L, 1, 2}, {L, 2, 1}, {L - 1, 2, 3}, {L - 1, 3, 2}];
neighbors(L, 1, 2) ->
    [{L, 1, 1}, {L, 1, 3}, {L, 2, 2}, {L - 1, 2, 3}];
neighbors(L, 1, 3) ->
    [{L, 1, 2}, {L, 1, 4}, {L, 2, 3}, {L - 1, 2, 3}];
neighbors(L, 1, 4) ->
    [{L, 1, 3}, {L, 1, 5}, {L, 2, 4}, {L - 1, 2, 3}];
neighbors(L, 1, 5) ->
    [{L, 1, 4}, {L, 2, 5}, {L - 1, 2, 3}, {L - 1, 3, 4}];

%% {2, 1}, {2, 3}, {2, 5}
neighbors(L, 2, 1) ->
    [{L, 1, 1}, {L, 2, 2}, {L, 3, 1}, {L - 1, 3, 2}];
neighbors(L, 2, 3) ->
    [{L, 1, 3}, {L, 2, 2}, {L, 2, 4},
     {L+1, 1, 1}, {L+1, 1, 2}, {L+1, 1, 3}, {L+1, 1, 4}, {L+1, 1, 5}];
neighbors(L, 2, 5) ->
    [{L, 1, 5}, {L, 2, 4}, {L, 3, 5}, {L - 1, 3, 4}];

%% {3, 1}, {3, 2}, {3, 4}, {3, 5}
neighbors(L, 3, 1) ->
    [{L, 2, 1}, {L, 3, 2}, {L, 4, 1}, {L - 1, 3, 2}];
neighbors(L, 3, 2) ->
    [{L, 2, 2}, {L, 3, 1}, {L, 4, 2},
     {L+1, 1, 1}, {L+1, 2, 1}, {L+1, 3, 1}, {L+1, 4, 1}, {L+1, 5, 1}];
neighbors(L, 3, 4) ->
    [{L, 2, 4}, {L, 3, 5}, {L, 4, 4},
     {L+1, 1, 5}, {L+1, 2, 5}, {L+1, 3, 5}, {L+1, 4, 5}, {L+1, 5, 5}];
neighbors(L, 3, 5) ->
    [{L, 2, 5}, {L, 3, 4}, {L, 4, 5}, {L - 1, 3, 4}];

%% {4, 1}, {4, 3}, {4, 5}
neighbors(L, 4, 1) ->
    [{L, 3, 1}, {L, 4, 2}, {L, 5, 1}, {L - 1, 3, 2}];
neighbors(L, 4, 3) ->
    [{L, 4, 2}, {L, 4, 4}, {L, 5, 3},
     {L+1, 5, 1}, {L+1, 5, 2}, {L+1, 5, 3}, {L+1, 5, 4}, {L+1, 5, 5}];
neighbors(L, 4, 5) ->
    [{L, 3, 5}, {L, 4, 4}, {L, 5, 5}, {L - 1, 3, 4}];

%% {5, X}
neighbors(L, 5, 1) ->
    [{L, 4, 1}, {L, 5, 2}, {L - 1, 3, 2}, {L - 1, 4, 3}];
neighbors(L, 5, 2) ->
    [{L, 4, 2}, {L, 5, 1}, {L, 5, 3}, {L - 1, 4, 3}];
neighbors(L, 5, 3) ->
    [{L, 4, 3}, {L, 5, 2}, {L, 5, 4}, {L - 1, 4, 3}];
neighbors(L, 5, 4) ->
    [{L, 4, 4}, {L, 5, 3}, {L, 5, 5}, {L - 1, 4, 3}];
neighbors(L, 5, 5) ->
    [{L, 4, 5}, {L, 5, 4}, {L - 1, 3, 4}, {L - 1, 4, 3}].
