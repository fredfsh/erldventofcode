-module(aoc_2018_12_1).

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
    case io:fread("", "initial state: ~s") of
        eof ->
            eof;
        {ok, [L]} ->
            L2 = lists:zip(L, lists:seq(0, length(L) - 1)),
            F = fun({$#, I}) ->
                        {true, I};
                   ({$., _}) ->
                        false
                end,
            L3 = lists:filtermap(F, L2),
            io:get_line(""),
            G = fun(_, Acc) ->
                        case io:fread("", "~s => ~c") of
                            {ok, [_, [$.]]} ->
                                Acc;
                            {ok, [LLCRR, [$#]]} ->
                                sets:add_element(LLCRR, Acc)
                        end
                end,
            Mapping = lists:foldl(G, sets:new(), lists:seq(1, 32)),
            {hd(L3), lists:last(L3), sets:from_list(L3), Mapping}
    end.

ini() ->
    0.

do({Min, Max, Pots, Mapping}) ->
    gen(0, Min, Max, Pots, Mapping).

-define(GENERATIONS, 20).

gen(?GENERATIONS, _, _, Pots, _) ->
    lists:sum(sets:to_list(Pots));
gen(Generation, Min, Max, Pots, Mapping) ->
    Init = {undefined, undefined, sets:new()},
    Range = lists:seq(Min - 2, Max + 2),
    F = fun(I, {MinAcc, MaxAcc, PotsAcc} = Acc) ->
                Key = [case sets:is_element(X, Pots) of
                           true ->
                               $#;
                           false ->
                               $.
                       end || X <- lists:seq(I - 2, I + 2)],
                case sets:is_element(Key, Mapping) of
                    true ->
                        {min(I, MinAcc),
                         case MaxAcc of undefined -> I; _ -> max(I, MaxAcc) end,
                         sets:add_element(I, PotsAcc)};
                    false ->
                        Acc
                end
        end,
    {NewMin, NewMax, NewPots} = lists:foldl(F, Init, Range),
    gen(Generation + 1, NewMin, NewMax, NewPots, Mapping).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
