-module(aoc_2018_20_1).

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
    case io:fread("", "^~s") of
        eof ->
            eof;
        {ok, [X]} ->
            input_impl([], X)
    end.

input_impl(Acc, [$$]) ->
    lists:reverse(Acc);
input_impl(Acc, [$( | T]) ->
    {Group, Rest} = input_group(T),
    input_impl([Group | Acc], Rest);
input_impl(Acc, [H | T]) ->
    input_impl([H | Acc], T).

input_group(X) ->
    input_group_impl([], [], X).

input_group_impl(Acc, Cur, [$) | T]) ->
    {lists:reverse([lists:reverse(Cur) | Acc]), T};
input_group_impl(Acc, Cur, [$| | T]) ->
    input_group_impl([lists:reverse(Cur) | Acc], [], T);
input_group_impl(Acc, Cur, [$( | T]) ->
    {Group, Rest} = input_group(T),
    input_group_impl(Acc, [Group | Cur], Rest);
input_group_impl(Acc, Cur, [H | T]) ->
    input_group_impl(Acc, [H | Cur], T).

ini() ->
    0.

do(X) ->
    {_, Doors} = walk(sets:from_list([{0, 0}]), sets:new(), X),
    bfs(Doors).

walk(Illusions, Doors, []) ->
    {Illusions, Doors};
walk(Illusions, Doors, [H | T]) ->
    F = fun(Illusion, Acc) ->
                move(Acc, Illusion, H)
        end,
    {NewIllusions, NewDoors} = sets:fold(F, {sets:new(), Doors}, Illusions),
    walk(NewIllusions, NewDoors, T).

move(Acc, XY, Choices) when is_list(Choices) ->
    F = fun(Choice, {IllusionsAcc, DoorsAcc}) ->
                SubIllusions = sets:from_list([XY]),
                {Illusions, Doors} = walk(SubIllusions, DoorsAcc, Choice),
                {sets:union(IllusionsAcc, Illusions), Doors}
        end,
    lists:foldl(F, Acc, Choices);
move({Illusions, Doors}, XY, Step) ->
    NXY = step(XY, Step),
    {sets:add_element(NXY, Illusions),
     sets:add_element({XY, NXY}, sets:add_element({NXY, XY}, Doors))}.

step({X, Y}, $N) ->
    {X, Y + 1};
step({X, Y}, $S) ->
    {X, Y - 1};
step({X, Y}, $W) ->
    {X - 1, Y};
step({X, Y}, $E) ->
    {X + 1, Y}.

bfs(Doors) ->
    bfs_impl(queue:from_list([{{0, 0}, 0}]), sets:from_list([{0, 0}]), Doors).

bfs_impl(Q, Visited, Doors) ->
    {{value, {XY, N}}, Q2} = queue:out(Q),
    Neighbors = [step(XY, D) || D <- [$N, $S, $W, $E]],
    F = fun(Neighbor, {QAcc, VisitedAcc} = Acc) ->
                case (not sets:is_element(Neighbor, VisitedAcc))
                    andalso sets:is_element({XY, Neighbor}, Doors) of
                    false ->
                        Acc;
                    true ->
                        {queue:in({Neighbor, N + 1}, QAcc),
                         sets:add_element(Neighbor, VisitedAcc)}
                end
        end,
    {NewQ, NewVisited} = lists:foldl(F, {Q2, Visited}, Neighbors),
    case queue:peek(NewQ) of
        empty ->
            N;
        _ ->
            bfs_impl(NewQ, NewVisited, Doors)
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
