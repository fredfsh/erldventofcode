-module(aoc_2025_4_1).

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
        {ok, [X]} ->
            X
    end.

ini() ->
    array:new().

do(X) ->
    array:from_list(X).

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(Graph) ->
    %%print(Graph),
    F = fun(Y, Arr, FAcc) ->
                G = fun(X, $@, GAcc) ->
                            case rolls(X, Y, Graph) < 4 of
                                true ->
                                    GAcc + 1;
                                false ->
                                    GAcc
                            end;
                       (_, $., GAcc) ->
                            GAcc
                    end,
                array:foldl(G, FAcc, Arr)
        end,
    array:foldl(F, 0, Graph).

-define(D, [{-1, -1}, { 0, -1}, { 1, -1},
            {-1,  0},           { 1,  0},
            {-1,  1}, { 0,  1}, { 1,  1}]).

-define(g(X, Y), array:get(X, array:get(Y, Graph))).

rolls(X, Y, Graph) ->
    Rows = array:size(Graph),
    Cols = array:size(array:get(0, Graph)),
    F = fun({DX, DY}, Acc) ->
                {NX, NY} = {X + DX, Y + DY},
                case NX < 0 orelse NX >= Cols orelse NY < 0 orelse NY >= Rows of
                    true ->
                        Acc;
                    false ->
                        case ?g(NX, NY) of
                            $@ ->
                                Acc + 1;
                            $. ->
                                Acc
                        end
                end
        end,
    lists:foldl(F, 0, ?D).

%% print(Graph) ->
%%     F = fun(_, Arr) -> io:format("~s~n", [array:to_list(Arr)]) end,
%%     array:map(F, Graph).
