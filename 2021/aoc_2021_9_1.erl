-module(aoc_2021_9_1).

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
            array:from_list([X - $0 || X <- L])
    end.

ini() ->
    array:new({default, array:new({default, undefined})}).

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Graph) ->
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, V, GAcc) ->
                            case low(X, Y, V, Graph) of
                                true ->
                                    GAcc + V + 1;
                                false ->
                                    GAcc
                            end
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, 0, Graph).

-define(D, [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]).

low(X, Y, V, Graph) ->
    F = fun({DX, DY}) ->
                {NX, NY} = {X + DX, Y + DY},
                NX >= 0 andalso NY >= 0 andalso height(NX, NY, Graph) =< V
        end,
    not lists:any(F, ?D).

height(X, Y, Graph) ->
    array:get(X, array:get(Y, Graph)).
