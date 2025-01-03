-module(aoc_2024_20_2).

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
            X
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    A = array:from_list(X),
    array:set(array:size(Acc), A, Acc).

-define(TAB, memo).
-define(SAVE, 100).

fin(Graph) ->
    {_, End} = se(Graph),

    ets:new(?TAB, [named_table]),
    ets:insert_new(?TAB, {End, 0}),
    bfs(queue:from_list([End]), Graph),
%%    io:format("~p~n", [ets:tab2list(?TAB)]),

    L = ets:tab2list(?TAB),
    length([undefined || {A, EA} <- L, {B, EB} <- L, cheat(A, EA, B, EB)]).
%%     F = fun({_, ETA}, Acc) when ETA < ?SAVE + 2 ->
%%                 Acc;
%%            ({XY, _}, Acc) ->
%%                 Acc + cheats(XY, Graph)
%%         end,
%%     ets:foldl(F, 0, ?TAB).

se(Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $S, {_, EAcc}) ->
                            {{X, Y}, EAcc};
                       (X, $E, {SAcc, _}) ->
                            {SAcc, {X, Y}};
                       (_, _, GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, {undefined, undefined}, Graph).

-define(D, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).
-define(WALL(X, Y), (array:get(X, array:get(Y, Graph)) =:= $#)).

bfs(Q, Graph) ->
    case queue:out(Q) of
        {empty, _} ->
            ok;
        {{value, {X, Y} = XY}, Q2} ->
            N = ets:lookup_element(?TAB, XY, 2),
            F = fun({DX, DY}, Acc) ->
                        {NX, NY} = NXY = {X + DX, Y + DY},
                        case ?WALL(NX, NY) orelse ets:member(?TAB, NXY) of
                            true ->
                                Acc;
                            false ->
                                ets:insert_new(?TAB, {NXY, N + 1}),
                                queue:in(NXY, Acc)
                        end
                end,
            NewQ = lists:foldl(F, Q2, ?D),
            bfs(NewQ, Graph)
    end.

-define(CHEAT, 20).

%% From discussion on Reddit, a cheat can activate not immediately before
%% entering a wall and terminate not immediately after coming out of a wall.
%%
%% e.g. this is a valid cheat with 11 picoseconds:
%%
%% ##################
%% #.....#####......#
%% #####.#####.######
%% #####.#####.######
%% #####.......######
%% ##################
%%
%% ##################
%% #..123456789AB...#
%% #####.#####.######
%% #####.#####.######
%% #####.......######
%% ##################
%%
%% If, instead, a cheat must activate right before entering a wall, and
%% terminate immediately after coming out a wall, then it can be solved
%% by the commented out code.  Which in my opinion, is more challenging
%% and reasonable.
cheat({XA, YA}, EA, {XB, YB}, EB) ->
    Cheat = abs(XB - XA) + abs(YB - YA),
    EA >= EB + ?SAVE + Cheat andalso Cheat =< ?CHEAT.

%% cheats({X, Y} = XY, Graph) ->
%%     ETA = ets:lookup_element(?TAB, XY, 2),
%%     Init = [{X + DX, Y + DY} || {DX, DY} <- ?D, ?WALL(X + DX, Y + DY)],
%%     Q = queue:from_list([{P, 1} || P <- Init]),
%%     bfs2(0, Q, sets:from_list(Init), ETA, Graph).

%% -define(DEPTH, 20).

%% bfs2(Acc, Q, Seen, ETA, Graph) ->
%%     Rows = array:size(Graph),
%%     Cols = array:size(array:get(0, Graph)),
%%     case queue:out(Q) of
%%         {empty, _} ->
%%             Acc;
%%         {{value, {_, Depth}}, _} when Depth >= ?DEPTH ->
%%             Acc;
%%         {{value, {{X, Y}, Depth}}, Q2} ->
%%             F = fun({DX, DY}, {CheatsAcc, QAcc, SeenAcc} = FAcc) ->
%%                         {NX, NY} = NXY = {X + DX, Y + DY},
%%                         case NX < 0 orelse NX >= Cols
%%                             orelse NY < 0 orelse NY >= Rows
%%                             orelse sets:is_element(NXY, SeenAcc) of
%%                             true ->
%%                                 FAcc;
%%                             false ->
%%                                 case ?WALL(NX, NY) of
%%                                     true ->
%%                                         {CheatsAcc,
%%                                          queue:in({NXY, Depth + 1}, QAcc),
%%                                          sets:add_element(NXY, SeenAcc)};
%%                                     false ->
%%                                         Cheat = count(NXY, Depth + 1, ETA),
%%                                         {CheatsAcc + Cheat,
%%                                          QAcc,
%%                                          sets:add_element(NXY, SeenAcc)}
%%                                 end
%%                         end
%%                 end,
%%             {NAcc, NQ, NSeen} = lists:foldl(F, {Acc, Q2, Seen}, ?D),
%%             bfs2(NAcc, NQ, NSeen, ETA, Graph)
%%     end.

%% count(XY, Depth, Original) ->
%%     case ets:lookup_element(?TAB, XY, 2, infinity) of
%%         Shortcut when Original - ?SAVE - Depth >= Shortcut ->
%%             1;
%%         _ ->
%%             0
%%     end.
