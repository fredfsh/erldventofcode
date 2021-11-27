-module(aoc_2019_12_2).

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
    case io:fread("", "<x=~d, y=~d, z=~d>") of
        eof ->
            eof;
        {ok, [X, Y, Z]} ->
            {X, Y, Z}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(Moons) ->
    {Xs, Ys, Zs} = lists:unzip3([{{X,0}, {Y,0}, {Z,0}} || {X, Y, Z} <- Moons]),
    repeat([cycle(Xs), cycle(Ys), cycle(Zs)]).

cycle(Moons) ->
    cycle_impl(0, maps:from_list([{Moons, 0}]), Moons).

cycle_impl(I, Seen, Moons) ->
    F = fun({X0, V}) ->
                G = fun({X, _}, VAcc) ->
                            pull(VAcc, X, X0)
                    end,
                NV = lists:foldl(G, V, Moons),
                {X0 + NV, NV}
        end,
    NewMoons = lists:map(F, Moons),
    case maps:get(NewMoons, Seen, undefined) of
        undefined ->
            cycle_impl(I + 1, maps:put(NewMoons, I + 1, Seen), NewMoons);
        J ->
            {I + 1 - J, J}
    end.

pull(V, X0, X0) ->
    V;
pull(V, X, X0) when X > X0 ->
    V + 1;
pull(V, X, X0) when X < X0 ->
    V - 1.

repeat(L) ->
    repeat_impl(0, 1, L).

repeat_impl(Acc, _, []) ->
    Acc;
repeat_impl(Acc, Cycle, [{C, I} | T]) when Acc >= I + C, (Acc - I) rem C =:= 0 ->
    repeat_impl(Acc, lcd(Cycle, C), T);
repeat_impl(Acc, Cycle, L) ->
    repeat_impl(Acc + Cycle, Cycle, L).

lcd(X, Y) ->
    X * Y div gcd(X, Y).

gcd(X, Y) when X < Y ->
    gcd(Y, X);
gcd(X, 0) ->
    X;
gcd(X, Y) ->
    gcd(Y, X rem Y).
