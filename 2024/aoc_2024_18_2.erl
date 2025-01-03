-module(aoc_2024_18_2).

-export([start/0]).

start() ->
    {X, Y} = run(),
    io:format("~p,~p~n", [X, Y]),
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
    case io:fread("", "~d,~d") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(R) ->
    Init = [{0, 0}],
    F = fun(_, {_, _} = Res) ->
                Res;
           (XY, Acc) ->
                Walls = sets:add_element(XY, Acc),
%%                io:format("adding ~p to wall~n", [XY]),
                case bfs(queue:from_list(Init), sets:from_list(Init), Walls) of
                    true ->
                        Walls;
                    false ->
                        XY
                end
        end,
    lists:foldl(F, sets:new(), lists:reverse(R)).

-define(N, 70).
-define(D, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).

bfs(Q, Seen, Walls) ->
    case queue:out(Q) of
        {empty, _} ->
            false;
        {{value, {?N, ?N}}, _} ->
            true;
        {{value, {X, Y}}, Q2} ->
            F = fun({DX, DY}, {QAcc, SeenAcc} = Acc) ->
                        {NX, NY} = NXY = {X + DX, Y + DY},
                        case NX < 0 orelse NX > ?N orelse NY < 0 orelse NY > ?N
                            orelse sets:is_element(NXY, SeenAcc)
                            orelse sets:is_element(NXY, Walls) of
                            true ->
                                Acc;
                            false ->
                                {queue:in(NXY, QAcc),
                                 sets:add_element(NXY, SeenAcc)}
                        end
                end,
            {NewQ, NewSeen} = lists:foldl(F, {Q2, Seen}, ?D),
            bfs(NewQ, NewSeen, Walls)
    end.
