-module(aoc_2024_15_2).

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
        {ok, [[$# | _] = L]} ->
            array:from_list(expand(L));
        {ok, [L]} ->
            L
    end.

expand(L) ->
    expand_impl([], L).

expand_impl(Acc, []) ->
    lists:reverse(Acc);
expand_impl(Acc, [$# | T]) ->
    expand_impl([$#, $# | Acc], T);
expand_impl(Acc, [$O | T]) ->
    expand_impl([$], $[ | Acc], T);
expand_impl(Acc, [$. | T]) ->
    expand_impl([$., $. | Acc], T);
expand_impl(Acc, [$@ | T]) ->
    expand_impl([$., $@ | Acc], T).

ini() ->
    {array:new(), []}.

do(X) ->
    X.

acc({Arr, L}, X) when is_list(X) ->
    {Arr, L ++ X};
acc({Arr, L}, X) ->
    {array:set(array:size(Arr), X, Arr), L}.

fin({Arr, Moves}) ->
    {Robot, Boxes, Walls} = graph(Arr),
    F = fun(Move, {RobotAcc, BoxesAcc}) ->
%%                print(RobotAcc, BoxesAcc, Walls),
                move(RobotAcc, BoxesAcc, Move, Walls)
        end,
    {_, NewBoxes} = lists:foldl(F, {Robot, Boxes}, Moves),
%%    print(NRobot, NewBoxes, Walls),
    gps(NewBoxes).

graph(Arr2) ->
%%    io:format("~p~n~p~n", [Arr2, array:to_list(Arr2)]),
    F = fun(Y, Arr, FAcc) ->
%%                io:format("~p: ~p~n", [Y, Arr]),
                G = fun(_, $., GAcc) ->
                            GAcc;
                       (X, $@, {_, BoxesAcc, WallsAcc}) ->
                            {{X, Y}, BoxesAcc, WallsAcc};
                       (X, $[, {RobotAcc, BoxesAcc, WallsAcc}) ->
                            Boxes = sets:add_element({X, Y}, BoxesAcc),
                            {RobotAcc, Boxes, WallsAcc};
                       (_, $], GAcc) ->
                            GAcc;
                       (X, $#, {RobotAcc, BoxesAcc, WallsAcc}) ->
                            Walls = sets:add_element({X, Y}, WallsAcc),
                            {RobotAcc, BoxesAcc, Walls}
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, {undefined, sets:new(), sets:new()}, Arr2).

-define(BL(X, Y), sets:is_element({X, Y}, Boxes)).
-define(BR(X, Y), sets:is_element({X - 1, Y}, Boxes)).
-define(BOX(X, Y), (?BL(X, Y) orelse ?BR(X, Y))).
-define(WALL(X, Y), sets:is_element({X, Y}, Walls)).

move({RX, RY} = RXY, Boxes, Move, Walls) ->
    {DX, DY} = d(Move),
    {NX, NY} = NXY = {RX + DX, RY + DY},
    case {?WALL(NX, NY), ?BOX(NX, NY)} of
        {false, false} ->
            {NXY, Boxes};
        {true, false} ->
            {RXY, Boxes};
        {false, true} ->
            push(NX, NY, DX, DY, Boxes, Walls)
    end.

d($^) -> { 0, -1};
d($v) -> { 0,  1};
d($<) -> {-1,  0};
d($>) -> { 1,  0}.

-define(BXY(X, Y), case ?BL(X, Y) of true -> {X, Y}; false -> {X - 1, Y} end).

push(NX, NY, DX, DY, Boxes, Walls) ->
%%    io:format("pushing ~p,~p from ~p,~p~n", [NX, NY, DX, DY]),
    BXY = ?BXY(NX, NY),
    Queue = queue:from_list([BXY]),
    Seen = sets:from_list([BXY]),
    case push_impl(Queue, Seen, DX, DY, Boxes, Walls) of
        undefined ->
            {{NX - DX, NY - DY}, Boxes};
        Pending ->
%%            io:format("pending: ~p~n", [Pending]),
            F = fun({X, Y}, Acc) ->
                        NAcc = sets:del_element({X, Y}, Acc),
                        sets:add_element({X + DX, Y + DY}, NAcc)
                end,
            {{NX, NY}, lists:foldl(F, Boxes, Pending)}
    end.

push_impl(Q, Seen, DX, DY, Boxes, Walls) ->
    case queue:out(Q) of
        {empty, _} ->
            F = fun({X1, Y1}, {X2, Y2}) ->
                        {X2 * DX, Y2 * DY} < {X1 * DX, Y1 * DY}
                end,
            lists:sort(F, sets:to_list(Seen));
        {{value, {X, Y}}, Q2} ->
            G = fun(_, undefined) ->
                        undefined;
                   ({XX, YY}, {QAcc, SeenAcc} = Acc) ->
                        {NX, NY} = {XX + DX, YY + DY},
                        case {?WALL(NX, NY), ?BOX(NX, NY)} of
                            {true, false} ->
                                undefined;
                            {false, true} ->
                                BXY = ?BXY(NX, NY),
                                case sets:is_element(BXY, SeenAcc) of
                                    true ->
                                        Acc;
                                    false ->
                                        {queue:in(BXY, QAcc),
                                         sets:add_element(BXY, SeenAcc)}
                                end;
                            {false, false} ->
                                Acc
                        end
                end,
            case lists:foldl(G, {Q2, Seen}, [{X, Y}, {X + 1, Y}]) of
                undefined ->
                    undefined;
                {NQ, NSeen} ->
                    push_impl(NQ, NSeen, DX, DY, Boxes, Walls)
            end
    end.

gps(Boxes) ->
    F = fun({X, Y}, Acc) ->
                Acc + 100 * Y + X
        end,
    sets:fold(F, 0, Boxes).

%% print({RX, RY}, Boxes, Walls) ->
%%     {Xs, Ys} = lists:unzip(sets:to_list(Walls)),
%%     {MX, MY} = {lists:max(Xs), lists:max(Ys)},
%%     FY = fun(Y) ->
%%                  FX = fun(X) when X =:= RX, Y =:= RY ->
%%                               io:format("@");
%%                          (X) ->
%%                               C = case {?WALL(X, Y), ?BL(X, Y), ?BR(X, Y)} of
%%                                       {true, false, false} ->
%%                                           $#;
%%                                       {false, true, false} ->
%%                                           $[;
%%                                       {false, false, true} ->
%%                                           $];
%%                                       {false, false, false} ->
%%                                           $.
%%                                   end,
%%                               io:format("~c", [C])
%%                       end,
%%                  lists:foreach(FX, lists:seq(0, MX)),
%%                  io:format("~n")
%%          end,
%%     lists:foreach(FY, lists:seq(0, MY)).
