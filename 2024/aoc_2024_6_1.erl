-module(aoc_2024_6_1).

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
        {ok, [L]} ->
            array:from_list(L)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Graph) ->
    {X, Y} = start(Graph),
    traverse(X, Y, 0, -1, sets:from_list([{X, Y}]), Graph).

start(Graph) ->
    G = fun(X, $^, undefined) ->
                X;
           (_, _, Acc) ->
                Acc
        end,
    F = fun(Y, Arr, undefined) ->
                case array:foldl(G, undefined, Arr) of
                    undefined ->
                        undefined;
                    X ->
                        {X, Y}
                end;
           (_, _, Res) ->
                Res
        end,
    array:foldl(F, undefined, Graph).

-define(a(X, Y), array:get(X, array:get(Y, Graph))).

traverse(X, Y, DX, DY, Visited, Graph) ->
    NX = X + DX,
    NY = Y + DY,
    case NX < 0 orelse NX >= array:size(array:get(0, Graph))
        orelse NY < 0 orelse NY >= array:size(Graph) of
        true ->
            sets:size(Visited);
        false ->
            case ?a(NX, NY) of
                $# ->
                    {NDX, NDY} = rturn(DX, DY),
                    traverse(X, Y, NDX, NDY, Visited, Graph);
                _ ->
                    NVisited = sets:add_element({NX, NY}, Visited),
                    traverse(NX, NY, DX, DY, NVisited, Graph)
            end
    end.

rturn( 1,  0) -> { 0,  1};
rturn( 0,  1) -> {-1,  0};
rturn(-1,  0) -> { 0, -1};
rturn( 0, -1) -> { 1,  0}.
