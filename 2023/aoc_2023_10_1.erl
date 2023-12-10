-module(aoc_2023_10_1).

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
    SXY = start(Arr),
    bfs(SXY, Arr).

start(Arr) ->
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

bfs(SXY, Arr) ->
    Nexts = snexts(SXY, Arr),
    bfs_impl(Nexts, sets:from_list([SXY]), Arr).

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
    maps:values(maps:filtermap(F, ?D)).

bfs_impl(Nexts, Seen, Arr) ->
%%    io:format("~p~n~p~n", [Nexts, Seen]),
    case Nexts of
        [XY, XY] ->
            (sets:size(Seen) + 1) div 2;
        [XY1, XY2] ->
            {Nexts1, Seen1} = nexts(XY1, Seen, Arr),
            {Nexts2, Seen2} = nexts(XY2, Seen1, Arr),
            bfs_impl(Nexts1 ++ Nexts2, Seen2, Arr)
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
