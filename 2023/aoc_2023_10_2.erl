-module(aoc_2023_10_2).

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
        {ok, [L]} ->
            array:from_list(string:trim(L))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Arr) ->
    {Loop, Arr2} = loop(Arr),
    F = fun(Y, A, FAcc) ->
                G = fun(X, _, GAcc) ->
                            case sets:is_element({X, Y}, Loop) of
                                true ->
                                    GAcc;
                                false ->
                                    case enclosed(X, Y, Loop, Arr2) of
                                        true ->
                                            GAcc + 1;
                                        false ->
                                            GAcc
                                    end
                            end
                    end,
                array:foldl(G, FAcc, A)
        end,
    array:foldl(F, 0, Arr2).

loop(Arr) ->
    SXY = locate(Arr),
    {Nexts, Arr2} = snexts(SXY, Arr),
    Loop = loop_impl(sets:from_list([SXY]), Nexts, Arr2),
    {Loop, Arr2}.

locate(Arr) ->
    G = fun(X, $S, undefined) ->
                X;
           (_, _, undefined) ->
                undefined;
           (_, _, GAcc) ->
                GAcc
        end,
    F = fun(Y, A, undefined) ->
                case array:foldl(G, undefined, A) of
                    undefined ->
                        undefined;
                    X ->
                        {X, Y}
                end;
           (_, _, FAcc) ->
                FAcc
        end,
    array:foldl(F, undefined, Arr).

-define(a(X, Y), array:get(X, array:get(Y, Arr))).

-define(D, #{{1, 0} => "-J7",
             {0, -1} => "|7F",
             {-1, 0} => "-LF",
             {0, 1} => "|LJ"}).

snexts({X, Y}, Arr) ->
    F = fun({DX, DY}, Allows) ->
                {NX, NY} = {X + DX, Y + DY},
                case lists:member(?a(NX, NY), Allows) of
                    true ->
                        {true, {NX, NY}};
                    false ->
                        false
                end
        end,
    Filtered = maps:filtermap(F, ?D),
    S = case maps:keys(Filtered) of
            [{0, 1}, {0, -1}] ->
                $|;
            [{0, -1}, {0, 1}] ->
                $|;
            [{1, 0}, {-1, 0}] ->
                $-;
            [{-1, 0}, {1, 0}] ->
                $-;
            [{0, -1}, {1, 0}] ->
                $L;
            [{1, 0}, {0, -1}] ->
                $L;
            [{0, -1}, {-1, 0}] ->
                $J;
            [{-1, 0}, {0, -1}] ->
                $J;
            [{0, 1}, {-1, 0}] ->
                $7;
            [{-1, 0}, {0, 1}] ->
                $7;
            [{0, 1}, {1, 0}] ->
                $F;
            [{1, 0}, {0, 1}] ->
                $F
        end,
    {maps:values(Filtered),
     array:set(Y, array:set(X, S, array:get(Y, Arr)), Arr)}.

loop_impl(Acc, Nexts, Arr) ->
    case Nexts of
        [XY, XY] ->
            sets:add_element(XY, Acc);
        [XY1, XY2] ->
            {Nexts1, Acc1} = nexts(XY1, Acc, Arr),
            {Nexts2, Acc2} = nexts(XY2, Acc1, Arr),
            loop_impl(Acc2, Nexts1 ++ Nexts2, Arr)
    end.

nexts({X, Y}, Seen, Arr) ->
    DXYs =
        case ?a(X, Y) of
            $| ->
                [{0, -1}, {0, 1}];
            $- ->
                [{-1, 0}, {1, 0}];
            $L ->
                [{0, -1}, {1, 0}];
            $J ->
                [{0, -1}, {-1, 0}];
            $7 ->
                [{-1, 0}, {0, 1}];
            $F ->
                [{1, 0}, {0, 1}]
        end,
    NewSeen = sets:add_element({X, Y}, Seen),
    F = fun({DX, DY}, Acc) ->
                NXY = {X + DX, Y + DY},
                case sets:is_element(NXY, Seen) of
                    true ->
                        Acc;
                    false ->
                        [NXY | Acc]
                end
        end,
    Nexts = lists:foldl(F, [], DXYs),
    {Nexts, NewSeen}.

-define(DD, [{1, 0, "|7F"}, {-1, 0, "|7F"}, {0, 1, "-LF"}, {0, -1, "-LF"}]).

enclosed(X, Y, Loop, Arr) ->
    Max = {array:size(array:get(0, Arr)) - 1, array:size(Arr) - 1},
    F = fun({DX, DY, Pipes}) ->
                E = edges(0, {X+DX, Y+DY}, {DX, DY}, Pipes, Max, Loop, Arr),
                E rem 2 =:= 1
        end,
    lists:any(F, ?DD).

edges(Acc, {X, Y}, _DXY, _Pipes, {XMax, YMax}, _Loop, _Arr)
  when X < 0; X > XMax; Y < 0; Y > YMax ->
    Acc;
edges(Acc, {X, Y}, {DX, DY} = DXY, Pipes, Max, Loop, Arr) ->
%%    io:format("~p ~p ~p ~p~n", [Acc, {X, Y}, DXY, Pipes]),
    NXY = {X + DX, Y + DY},
    case sets:is_element({X, Y}, Loop) of
        false ->
            edges(Acc, NXY, DXY, Pipes, Max, Loop, Arr);
        true ->
            case lists:member(?a(X, Y), Pipes) of
                true ->
                    edges(Acc + 1, NXY, DXY, Pipes, Max, Loop, Arr);
                _ ->
                    edges(Acc, NXY, DXY, Pipes, Max, Loop, Arr)
            end
    end.
