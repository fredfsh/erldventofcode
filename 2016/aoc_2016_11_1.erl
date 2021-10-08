-module(aoc_2016_11_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    {Elements, Init} = input(),
    Goal = goal(Elements),
    bfs(sets:to_list(Elements), Init, Goal).

input() ->
    Init = maps:put(elevator, 1, maps:new()),
    F = fun(Floor, {ElementsAcc, InitAcc}) ->
                case io:fread("", "The ~s floor contains ~a") of
                    {ok, [_, nothing]} ->
                        io:fread("", " relevant."),
                        {ElementsAcc, InitAcc};
                    {ok, [_, a]} ->
                        input_impl(Floor, ElementsAcc, InitAcc)
                end
        end,
    lists:foldl(F, {sets:new(), Init}, lists:seq(1, 4)).

input_impl(Floor, ElementsAcc, InitAcc) ->
    {ok, [S1, S2]} = io:fread("", " ~s ~s"),
    {NewElements, NewInit} = input_device(Floor, S1, S2, ElementsAcc, InitAcc),
    case lists:last(S2) of
        $. ->
            {NewElements, NewInit};
        _ ->
            case io:fread("", " ~a") of
                {ok, ['and']} ->
                    {ok, [a]} = io:fread("", " ~a");
                {ok, [a]} ->
                    ok
            end,
            input_impl(Floor, NewElements, NewInit)
    end.

input_device(Floor, ElementStr, DeviceStr, Elements, Init) ->
    DeviceS =
        case lists:last(DeviceStr) of
            A when A >= $a, A =< $z ->
                DeviceStr;
            _ ->
                lists:droplast(DeviceStr)
        end,
    {Element, Device} =
        case DeviceS of
            "generator" ->
                {list_to_atom(ElementStr), generator};
            "microchip" ->
                {RealElementStr, _} = string:take(ElementStr, lists:seq($a, $z)),
                {list_to_atom(RealElementStr), microchip}
        end,
    {sets:add_element(Element, Elements),
     maps:put({Element, Device}, Floor, Init)}.

goal(Elements) ->
    M0 = maps:new(),
    M1 = maps:put(elevator, 4, M0),
    F = fun(Element, Acc) ->
                AccAcc = maps:put({Element, generator}, 4, Acc),
                maps:put({Element, microchip}, 4, AccAcc)
        end,
    sets:fold(F, M1, Elements).

bfs(Elements, Init, Goal) ->
    Q = queue:in(Init, queue:in(0, queue:new())),
    Set = sets:add_element(Init, sets:new()),
    bfs_impl(Elements, Goal, -1, Q, Set).

bfs_impl(Elements, Goal, N, Q, Closed) ->
    case queue:out(Q) of
        {{value, Goal}, _} ->
            N;
        {{value, X}, Q2} when X =:= N + 1 ->
            bfs_impl(Elements, Goal, N + 1, queue:in(N + 2, Q2), Closed);
        {{value, State}, Q2} ->
            States = neighbors(Elements, State),
            F = fun(S, {QAcc, ClosedAcc} = Acc) ->
                        case sets:is_element(S, ClosedAcc) of
                            false ->
                                {queue:in(S, QAcc),
                                 sets:add_element(S, ClosedAcc)};
                            true ->
                                Acc
                        end
                end,
            {NewQ, NewClosed} = lists:foldl(F, {Q2, Closed}, States),
            bfs_impl(Elements, Goal, N, NewQ, NewClosed)
    end.

neighbors(Elements, State) ->
    Elvl = maps:get(elevator, State),
    NextElvls = case Elvl of
                    4 ->
                        [3];
                    1 ->
                        [2];
                    N ->
                        [N + 1, N - 1]
                end,
    Devices = at(Elvl, State),
    Couples = [[X, Y] || X <- Devices, Y <- Devices, X < Y],
    neighbors_impl(Elements, State, Devices ++ Couples, NextElvls, []).

at(Lvl, State) ->
    F = fun(K, V) -> K =/= elevator andalso V =:= Lvl end,
    maps:keys(maps:filter(F, State)).

neighbors_impl(_Elements, _State, _Whats, [], Acc) ->
    Acc;
neighbors_impl(Elements, State, Whats, [H|T], Acc) ->
    State1 = maps:put(elevator, H, State),
    F = fun(What) ->
                Candidate = case What of
                                [D1, D2] ->
                                    maps:put(D2, H, maps:put(D1, H, State1));
                                D ->
                                    maps:put(D, H, State1)
                            end,
                case legal(Elements, Candidate) of
                    true ->
                        {true, Candidate};
                    false ->
                        false
                end
        end,
    NewAcc = Acc ++ lists:filtermap(F, Whats),
    neighbors_impl(Elements, State, Whats, T, NewAcc).

legal(Elements, S) ->
    Combos = [{X, Y} || X <- Elements, Y <- Elements, X =/= Y],
    F = fun({X, Y}) ->
                Lvl = maps:get({X, microchip}, S),
                Lvl =/= maps:get({X, generator}, S)
                    andalso Lvl =:= maps:get({Y, generator}, S)
        end,
    not lists:any(F, Combos).
