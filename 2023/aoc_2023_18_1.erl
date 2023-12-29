-module(aoc_2023_18_1).

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
    case io:fread("", "~a ~d ~s") of
        eof ->
            eof;
        {ok, [Dir, Len, _]} ->
            {Dir, Len}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    area(dig(lists:reverse(X))).

dig(Plans) ->
    dig_impl(0, 0, 0, 0, sets:from_list([{0, 0}]), 0, 0, Plans).

dig_impl(MinX, MinY, MaxX, MaxY, Trenches, _, _, []) ->
    {MinX, MinY, MaxX, MaxY, Trenches};
dig_impl(MinX, MinY, MaxX, MaxY, Trenches, X, Y, [{Dir, Len} | T]) ->
    {DX, DY} = dir(Dir),
    F = fun(I, Acc) ->
                NXY = {X + DX * I, Y + DY * I},
                sets:add_element(NXY, Acc)
        end,
    NewTrenches = lists:foldl(F, Trenches, lists:seq(1, Len)),
    MX = X + DX * Len,
    MY = Y + DY * Len,
    NMinX = min(MinX, MX),
    NMinY = min(MinY, MY),
    NMaxX = max(MaxX, MX),
    NMaxY = max(MaxY, MY),
    dig_impl(NMinX, NMinY, NMaxX, NMaxY, NewTrenches, MX, MY, T).

dir('U') -> { 0, -1};
dir('D') -> { 0,  1};
dir('L') -> {-1,  0};
dir('R') -> { 1,  0}.

area({MinX, MinY, MaxX, MaxY, _} = Meta) ->
    Top = [{X, MinY} || X <- lists:seq(MinX, MaxX)],
    Bottom = [{X, MaxY} || X <- lists:seq(MinX, MaxX)],
    Left = [{MinX, Y} || Y <- lists:seq(MinY, MaxY)],
    Right = [{MaxX, Y} || Y <- lists:seq(MinY, MaxY)],
    F = fun({X, Y}, Acc) ->
                floodfill(Acc, X, Y, Meta)
        end,
    Exterior = lists:foldl(F, sets:new(), lists:append([Top,Bottom,Left,Right])),
    (MaxX - MinX + 1) * (MaxY - MinY + 1) - sets:size(Exterior).

floodfill(Exterior, X, Y, {_, _, _, _, Trenches} = Meta) ->
    case sets:is_element({X, Y}, Trenches) of
        true ->
            Exterior;
        false ->
            Q = queue:from_list([{X, Y}]),
            floodfill_impl(Q, sets:add_element({X, Y}, Exterior), Meta)
    end.

-define(D, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).

floodfill_impl(Q, Exterior, {MinX, MinY, MaxX, MaxY, Trenches} = Meta) ->
    case queue:out(Q) of
        {empty, _} ->
            Exterior;
        {{value, {X, Y}}, Q2} ->
            F = fun({DX, DY}, {QAcc, ExteriorAcc} = Acc) ->
                        {NX, NY} = NXY = {X + DX, Y + DY},
                        case NX >= MinX andalso NX =< MaxX
                            andalso NY >= MinY andalso NY =< MaxY
                            andalso not sets:is_element(NXY, Trenches)
                            andalso not sets:is_element(NXY, Exterior) of
                            true ->
                                {queue:in(NXY, QAcc),
                                 sets:add_element(NXY, ExteriorAcc)};
                            false ->
                                Acc
                        end
                end,
            {NQ, NExterior} = lists:foldl(F, {Q2, Exterior}, ?D),
            floodfill_impl(NQ, NExterior, Meta)
    end.
