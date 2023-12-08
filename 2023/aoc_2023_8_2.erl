-module(aoc_2023_8_2).

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
    {Instruction, [], maps:new()}.

do(X) ->
    X.

acc({Instruction, Starts, Map}, {Node, LeftNode, RightNode}) ->
    NewStarts =
        case Node of
            [_, _, $A] ->
                [Node | Starts];
            _ ->
                Starts
        end,
    {Instruction, NewStarts, maps:put(Node, {LeftNode, RightNode}, Map)}.

fin({Instruction, Starts, Map}) ->
    %% "the number of nodes with names ending in A is equal to the number ending in Z!"
    %% This implies there are N non-overlapping cycles where N is the number of A- nodes.
    Cycles = [cycle(Start, Instruction, Map) || Start <- Starts],
    steps(Cycles).

cycle(Start, Instruction, Map) ->
    cycle_impl(0, undefined, Start, Instruction, Instruction, Map).

cycle_impl(Acc, Init, Node, [], Instruction, Map) ->
    cycle_impl(Acc, Init, Node, Instruction, Instruction, Map);
cycle_impl(Acc, undefined, [_, _, $Z] = Node, [H | T], Instruction, Map) ->
    cycle_impl(Acc + 1, Acc, next(Node, H, Map), T, Instruction, Map);
cycle_impl(Acc, Init, [_, _, $Z], _, _, _) ->
    {Init, Acc - Init};
cycle_impl(Acc, Init, Node, [H | T], Instruction, Map) ->
    cycle_impl(Acc + 1, Init, next(Node, H, Map), T, Instruction, Map).

next(Node, $L, Map) ->
    element(1, maps:get(Node, Map));
next(Node, $R, Map) ->
    element(2, maps:get(Node, Map)).

steps(Cycles) ->
%%    io:format("~p~n", [Cycles]),
    F = fun({Init, Cycle}, {Acc, Multiplier}) ->
%%                io:format("~p,~p ~p,~p~n", [Acc, Multiplier, Init, Cycle]),
                {search(Acc, Multiplier, Init, Cycle), lcm(Cycle, Multiplier)}
        end,
    {Res, _} = lists:foldl(F, {1, 1}, Cycles),
    Res.

search(Acc, _, Init, Cycle) when (Acc - Init) rem Cycle =:= 0 ->
    Acc;
search(Acc, Multiplier, Init, Cycle) ->
    search(Acc + Multiplier, Multiplier, Init, Cycle).

lcm(A, B) ->
    A * B div gcd(A, B).

gcd(A, B) when A < B ->
    gcd(B, A);
gcd(A, B) when A rem B =:= 0 ->
    B;
gcd(A, B) ->
    gcd(B, A rem B).
