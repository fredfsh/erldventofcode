-module(aoc_2024_23_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~s~n", [Out]),
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
        {ok, [X]} ->
            [A, B] = string:lexemes(X, "-"),
            {A, B}
    end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, {A, B}) ->
    ln(Acc, A, B).

ln(Acc, A, B) when A > B ->
    ln(Acc, B, A);
ln(Acc, A, B) ->
    F = fun(V) -> sets:add_element(B, V) end,
    maps:update_with(A, F, sets:from_list([B]), Acc).

fin(Links) ->
    Complete = complete(Links),
    lists:join($,, lists:reverse(Complete)).

%% keep a set of all complete subgraphs of size N,
%% gradually increase N until there's one element left in the set,
%% the subgraph is represented by a list of nodes ordered by their names,
%% turns out this is efficient enough for searching the result
complete(Links) ->
%%    io:format("~p~n", [Links]),
    Init = double(Links),
    complete_impl(Init, Links).

double(Links) ->
    F = fun(K, V, FAcc) ->
                G = fun(X, GAcc) ->
                            [[X, K] | GAcc]
                    end,
                sets:fold(G, FAcc, V)
        end,
    maps:fold(F, [], Links).

complete_impl([Res], _) ->
    Res;
complete_impl(Completes, Links) ->
    F = fun([H | T] = Complete, FAcc) ->
                G = fun(Node, GAcc) ->
                            case fully_connected(Node, T, Links) of
                                true ->
                                    [[Node | Complete] | GAcc];
                                false ->
                                    GAcc
                            end
                    end,
                sets:fold(G, FAcc, maps:get(H, Links, sets:new()))
        end,
    complete_impl(lists:foldl(F, [], Completes), Links).

fully_connected(Node, Graph, Links) ->
    F = fun(X) -> connected(Node, X, Links) end,
    lists:all(F, Graph).

connected(A, B, Links) when A > B ->
    connected(B, A, Links);
connected(A, B, Links) ->
    sets:is_element(B, maps:get(A, Links)).
