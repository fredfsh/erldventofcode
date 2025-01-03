-module(aoc_2024_20_1).

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
    {Start, End} = se(Graph),

    ets:new(?TAB, [named_table]),
    ets:insert_new(?TAB, {End, 0}),
    bfs(queue:from_list([End]), Graph),
%%    io:format("~p~n", [ets:tab2list(?TAB)]),

    Picoseconds = ets:lookup_element(?TAB, Start, 2),
    shortcuts(Start, Picoseconds - ?SAVE, Graph).

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

shortcuts(Start, Budget, Graph) ->
    bfs2(0, queue:from_list([{Start, Budget}]), sets:from_list([Start]), Graph).

bfs2(Acc, Q, Seen, Graph) ->
    Rows = array:size(Graph),
    Cols = array:size(array:get(0, Graph)),
    case queue:out(Q) of
        {empty, _} ->
            Acc;
        {{value, {{X, Y}, Budget}}, Q2} ->
            F = fun({DX, DY}, {ShortcutsAcc, QAcc, SeenAcc}) ->

                        {NX, NY} = NXY = {X + DX, Y + DY},
                        {NQ, NSeen} =
                            case ?WALL(NX, NY)
                                orelse sets:is_element(NXY, SeenAcc) of
                                true ->
                                    {QAcc, SeenAcc};
                                false ->
                                    {queue:in({NXY, Budget - 1}, QAcc),
                                     sets:add_element(NXY, SeenAcc)}
                            end,

                        {NNX, NNY} = NNXY = {NX + DX, NY + DY},
                        ETA = ets:lookup_element(?TAB, NNXY, 2, infinity),
                        Shortcuts = case NNX >=0 andalso NNX < Cols
                                        andalso NNY >= 0 andalso NNY < Rows
                                        andalso not ?WALL(NNX, NNY)
                                        andalso ETA =< Budget - 2
                                        andalso ?WALL(NX, NY) of
                                        true ->
                                            ShortcutsAcc + 1;
                                        false ->
                                            ShortcutsAcc
                                    end,

%%                        X =:= 7 andalso Y =:= 7 andalso io:format("nxy: ~p nnxy: ~p shortcuts: ~p eta: ~p budget: ~p ~n", [NXY, NNXY, Shortcuts, ETA, Budget]),
                        {Shortcuts, NQ, NSeen}
                end,
            {NewAcc, NewQ, NewSeen} = lists:foldl(F, {Acc, Q2, Seen}, ?D),
            bfs2(NewAcc, NewQ, NewSeen, Graph)
    end.
