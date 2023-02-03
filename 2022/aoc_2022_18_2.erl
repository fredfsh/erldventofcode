-module(aoc_2022_18_2).

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
    %% observe no negative coordinates
    {sets:new(), {-1, undefined, -1, undefined, -1, undefined}}.

do(X) ->
    X.

acc({Set, {MinX, MaxX, MinY, MaxY, MinZ, MaxZ}}, {X, Y, Z}) ->
    {sets:add_element({X, Y, Z}, Set),
     {min(MinX, X), max(MaxX, X),
      min(MinY, Y), max(MaxY, Y),
      min(MinZ, Z), max(MaxZ, Z)}}.

fin(X) ->
    fin_impl(fill(X)).

-define(D, [{-1,  0,  0},
            { 1,  0,  0},
            { 0, -1,  0},
            { 0,  1,  0},
            { 0,  0, -1},
            { 0,  0,  1}]).

fill({Set, MinMax}) ->
    F = fun({X, Y, Z}, Acc) ->
                G = fun({DX, DY, DZ}, GAcc) ->
                            Filled = fill_impl({X+DX, Y+DY, Z+DZ}, GAcc, MinMax),
                            sets:union(Filled, GAcc)
                    end,
                lists:foldl(G, Acc, ?D)
        end,
    sets:fold(F, Set, Set).

fill_impl(XYZ, Set, MinMax) ->
    case sets:is_element(XYZ, Set) of
        true ->
            sets:new();
        false ->
            L = [XYZ],
            floodfill(queue:from_list(L), sets:from_list(L), Set, MinMax)
    end.

floodfill(Q, Filled, Set, {MinX, MaxX, MinY, MaxY, MinZ, MaxZ} = MinMax) ->
    case queue:out(Q) of
        {empty, _} ->
            Filled;
        {{value, {X, Y, Z}}, Q2} ->
            F = fun(_, external) ->
                        external;
                   ({DX, DY, DZ}, {QAcc, FAcc}) ->
                        case {X + DX, Y + DY, Z + DZ} of
                            {NX, NY, NZ} when NX < MinX; NX > MaxX;
                                              NY < MinY; NY > MaxY;
                                              NZ < MinZ; NZ > MaxZ ->
                                external;
                            NXYZ ->
                                case sets:is_element(NXYZ, Filled) orelse
                                    sets:is_element(NXYZ, Set) of
                                    true ->
                                        {QAcc, FAcc};
                                    false ->
                                        {queue:in(NXYZ, QAcc),
                                         sets:add_element(NXYZ, Filled)}
                                end
                        end
                end,
            case lists:foldl(F, {Q2, Filled}, ?D) of
                external ->
                    sets:new();
                {NQ, NFilled} ->
                    floodfill(NQ, NFilled, Set, MinMax)
            end
    end.

fin_impl(Set) ->
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
