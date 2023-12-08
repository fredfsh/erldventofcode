-module(aoc_2023_8_1).

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
    case io:fread("", "~s = ~s ~s") of
        eof ->
            eof;
        {ok, [Node, Left, Right]} ->
%%            io:format("~p ~p ~n", [Left, Right]),
            LeftNode = string:slice(Left, 1, 3),
            RightNode = string:slice(Right, 0, 3),
%%            io:format("~p ~p ~n", [LeftNode, RightNode]),
            {Node, LeftNode, RightNode}
    end.

ini() ->
    {ok, [Instruction]} = io:fread("", "~s"),
    {Instruction, maps:new()}.

do(X) ->
    X.

acc({Instruction, Map}, {Node, LeftNode, RightNode}) ->
    {Instruction, maps:put(Node, {LeftNode, RightNode}, Map)}.

fin({Instruction, Map}) ->
    fin_impl(0, "AAA", Instruction, Instruction, Map).

fin_impl(Acc, "ZZZ", _, _, _) ->
    Acc;
fin_impl(Acc, Node, [], Instruction, Map) ->
    fin_impl(Acc, Node, Instruction, Instruction, Map);
fin_impl(Acc, Node, [$L | T], Instruction, Map) ->
    fin_impl(Acc + 1, element(1, maps:get(Node, Map)), T, Instruction, Map);
fin_impl(Acc, Node, [$R | T], Instruction, Map) ->
    fin_impl(Acc + 1, element(2, maps:get(Node, Map)), T, Instruction, Map).
