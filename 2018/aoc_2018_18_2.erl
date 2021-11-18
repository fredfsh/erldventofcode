-module(aoc_2018_18_2).

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
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), array:from_list(X), Acc).

fin(X) ->
    conway(0, X, maps:new(), maps:new()).

-define(GENERATIONS, 1000000000).

conway(I, Arr, Generations, Values) ->
    case maps:get(Arr, Generations, undefined) of
        undefined ->
            NewValues = maps:put(I, count(Arr, $|) * count(Arr, $#), Values),
            F = fun(Y, Row) ->
                        G = fun(X, C) -> transform(X, Y, C, Arr) end,
                        array:map(G, Row)
                end,
            NewArr = array:map(F, Arr),
            conway(I + 1, NewArr, maps:put(Arr, I, Generations), NewValues);
        J ->
            Gen = (?GENERATIONS - J) rem (I - J) + J,
            maps:get(Gen, Values)
    end.

count(Arr, C) ->
    G = fun(_, What, Acc) when What =:= C -> Acc + 1; (_, _, Acc) -> Acc end,
    F = fun(_, Row, Acc) -> array:foldl(G, Acc, Row) end,
    array:foldl(F, 0, Arr).

-define(D, [{-1, -1}, { 0, -1}, { 1, -1},
            {-1,  0},           { 1,  0},
            {-1,  1}, { 0,  1}, { 1,  1}]).

transform(X, Y, C, Arr) ->
    M = array:size(Arr),
    N = array:size(array:get(0, Arr)),
    F = fun({DX, DY}, {OAcc, TAcc, LAcc} = Acc) ->
                {XX, YY} = {X + DX, Y + DY},
                case XX >= 0 andalso XX < N andalso YY >= 0 andalso YY < M of
                    false ->
                        Acc;
                    true ->
                        case array:get(XX, array:get(YY, Arr)) of
                            $. ->
                                {OAcc + 1, TAcc, LAcc};
                            $| ->
                                {OAcc, TAcc + 1, LAcc};
                            $# ->
                                {OAcc, TAcc, LAcc + 1}
                        end
                end
        end,
    case {C, lists:foldl(F, {0, 0, 0}, ?D)} of
        {$., {_, Trees, _}} when Trees >= 3 ->
            $|;
        {$., _} ->
            $.;
        {$|, {_, _, Lumberyards}} when Lumberyards >= 3 ->
            $#;
        {$|, _} ->
            $|;
        {$#, {_, Trees, Lumberyards}} when Trees >= 1, Lumberyards >= 1 ->
            $#;
        {$#, _} ->
            $.
    end.
