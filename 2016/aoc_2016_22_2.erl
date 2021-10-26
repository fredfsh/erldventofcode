-module(aoc_2016_22_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    io:get_line(""),
    io:get_line(""),
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "/dev/grid/node-x~d-y~d ~dT ~dT ~dT ~d%") of
        eof ->
            Acc;
        {ok, [X, Y, _, Used, Avail, _]} ->
            input_impl([{{X, Y}, Used, Avail} | Acc])
    end.

%% Observations on input:
%%   - Very large nodes (#) are from (13, 19) to (37, 19), they can't be moved
%%   - Just one empty node (_) at (35, 24)
%%   - Other nodes (.) 1/ are large enough to hold the other's data;
%%                     2/ their data are small enough to be held in other nodes
%%                     3/ their available spaces are small enough such that they
%%                        must be moved first to hold other nodes data
%%
%% The strategy is:
%%    1a. Move empty node from (35, 24) to (36, 0) without touching large nodes
%%    1b. Move empty node from (36,  0) to (37, 0), i.e. G moving to (36, 0)
%%    2a. Move empty node from (37,  0) to (35, 0) without touching G at (36, 0)
%%    2b. Move empty node from (35,  0) to (36, 0), i.e. G moving to (35, 0)
%%   ...
%%   37a. Move empty node from ( 2,  0) to ( 0, 0) without touching G at ( 1, 0)
%%   37b. Move empty node from ( 0,  0) to ( 1, 0), i.e. G moving to ( 0, 0)
%%
%% Calculation:
%%   - Step 1a can be calculated with floodfill.
%%   - Step 1b takes 1 move.
%%   - Large nodes are irrelevant for steps 2-37, each taking 5 moves.

do(X) ->
    MaxX = max_x(X),
    step1a(X, MaxX) + 1 + (MaxX - 1) * 5.

max_x(Nodes) ->
    lists:max([X || {{X, _}, _, _} <- Nodes]).

step1a(Nodes, MaxX) ->
    EmptyNode = empty_node(Nodes),
    RemainingNodes = node_map(Nodes, MaxX, EmptyNode),
    Q = queue:in({EmptyNode, 0}, queue:new()),
    floodfill(Q, RemainingNodes, {MaxX - 1, 0}).

empty_node(Nodes) ->
    {value, {EmptyNode, _, _}} = lists:keysearch(0, 2, Nodes),
    EmptyNode.

node_map(Nodes, MaxX, {EmptyX, EmptyY}) ->
    F = fun({{X, 0}, _, _}) when X =:= MaxX -> false;
           ({{X, Y}, _, _}) when X =:= EmptyX, Y =:= EmptyY -> false;
           ({{_, _}, Used, _}) when Used >= 100 -> false;
           ({{X, Y}, Used, Avail}) ->  {true, {{X, Y}, {Used, Avail}}}
        end,
    maps:from_list(lists:filtermap(F, Nodes)).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

floodfill(Q, Nodes, TargetNode) ->
    {{value, {{X, Y}, N}}, Q2} = queue:out(Q),
    Candidates = [{DX + X, DY + Y} || {DX, DY} <- ?D],
    F = fun(Node, _) when Node =:= TargetNode ->
                N + 1;
           (Node, {QAcc, NodesAcc}) ->
                case maps:is_key(Node, NodesAcc) of
                    true ->
                        {queue:in({Node, N + 1}, QAcc),
                         maps:remove(Node, NodesAcc)};
                    false ->
                        {QAcc, NodesAcc}
                end;
           (_, M) ->
                M
        end,
    case lists:foldl(F, {Q2, Nodes}, Candidates) of
        M when is_integer(M) ->
            M;
        {NewQ, NewNodes} ->
            floodfill(NewQ, NewNodes, TargetNode)
    end.
