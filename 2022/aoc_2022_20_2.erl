-module(aoc_2022_20_2).

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
    case io:fread("", "~d") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    {maps:new(), 0}.

do(X) ->
    X.

-record(node, {id, val, prev, next}).

-define(KEY, 811589153).
acc({Map, ID}, X) ->
    {maps:put(ID, #node{id=ID, val=X*?KEY, prev=ID-1, next=ID+1}, Map), ID + 1}.

-define(MIXES, 10).
fin({Map, N}) ->
    F = fun(#node{id = 0} = Node) ->
                Node#node{prev = N - 1};
           (#node{id = ID} = Node) when ID =:= N - 1 ->
                Node#node{next = 0}
        end,
    Map1 = maps:update_with(0, F, Map),
    Map2 = maps:update_with(N - 1, F, Map1),
    Map3 = mix(?MIXES, Map2),
    nth(1000, Map3) + nth(2000, Map3) + nth(3000, Map3).

mix(0, Map) ->
    Map;
mix(N, Map) ->
    mix(N - 1, mix_impl(0, Map)).

mix_impl(ID, Map) when ID =:= map_size(Map) ->
    Map;
mix_impl(ID, Map) ->
    #node{val = Val, prev = Prev, next = Next} = Node = maps:get(ID, Map),
    F = fun(#node{id = I} = N) when I =:= Prev ->
                N#node{next = Next};
           (#node{id = I} = N) when I =:= Next ->
                N#node{prev = Prev}
        end,
    Map1 = maps:update_with(Prev, F, Map),
    Map2 = maps:update_with(Next, F, Map1),

    C = map_size(Map2) - 1,
    Move = (Val rem C + C) rem C,
    #node{id = AID, next = ANext} = move(Move, maps:get(Prev, Map2), Map2),
    Map3 = Map2#{ID => Node#node{prev = AID, next = ANext}},
    G = fun(#node{id = I} = N) when I =:= AID ->
                N#node{next = ID};
           (#node{id = I} = N) when I =:= ANext ->
                N#node{prev = ID}
        end,
    Map4 = maps:update_with(AID, G, Map3),
    Map5 = maps:update_with(ANext, G, Map4),

    mix_impl(ID + 1, Map5).

move(0, Node, _) ->
    Node;
move(N, #node{next = Next}, Map) ->
    move(N - 1, maps:get(Next, Map), Map).

nth(N, Map) ->
    nth_impl(N, node0(Map), Map).

node0(Map) ->
    F = fun(_, #node{val = 0} = Node, undefined) ->
                Node;
           (_, _, undefined) ->
                undefined;
           (_, _, Node) ->
                Node
        end,
    maps:fold(F, undefined, Map).

nth_impl(0, #node{val = Val}, _) ->
    Val;
nth_impl(N, #node{next = Next}, Map) ->
    nth_impl(N - 1, maps:get(Next, Map), Map).
