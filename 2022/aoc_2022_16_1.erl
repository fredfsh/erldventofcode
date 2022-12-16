-module(aoc_2022_16_1).

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
    Fmt = "Valve ~a has flow rate=~d; ~s ~s to ~s",
    case io:fread("", Fmt) of
        eof ->
            eof;
        {ok, [Valve, Flowrate, _, _, _]} ->
            Line = string:trim(io:get_line("")),
            Parts = string:split(Line, ", ", all),
            {Valve, Flowrate, [list_to_atom(P) || P <- Parts]}
    end.

ini() ->
    {sets:new(), maps:new(), sets:new(), maps:new()}.

do(X) ->
    X.

acc({Valves, Flowrates, PipeSets, PipeMaps}, {Valve, Flowrate, Dests}) ->
    NFlowrates = case Flowrate of
                     0 ->
                         Flowrates;
                     _ ->
                         maps:put(Valve, Flowrate, Flowrates)
                 end,
    {sets:add_element(Valve, Valves),
     NFlowrates,
     sets:union(sets:from_list([{Valve, D} || D <- Dests]), PipeSets),
     maps:put(Valve, Dests, PipeMaps)}.

-define(MINUTES, 30).
-define(START, 'AA').

fin({Valves, Flowrates, _PipeSets, PipeMaps}) ->
    Steps = steps(Valves, PipeMaps),
    dfs(Flowrates, ?MINUTES, ?START, 0, Steps).

steps(Valves, PipeMaps) ->
    F = fun(Valve, Acc) ->
                steps_impl(queue:from_list([{Valve, 0}]),
                           sets:from_list([Valve]),
                           Acc,
                           Valve,
                           PipeMaps)
        end,
    sets:fold(F, maps:new(), Valves).

steps_impl(Q, Visited, Steps, From, PipeMaps) ->
    case queue:out(Q) of
        {empty, _} ->
            Steps;
        {{value, {Valve, N}}, Q2} ->
            F = fun(Dest, {QAcc, VAcc, StepsAcc} = Acc) ->
                        case sets:is_element(Dest, VAcc) of
                            true ->
                                Acc;
                            false ->
                                {queue:in({Dest, N + 1}, QAcc),
                                 sets:add_element(Dest, VAcc),
                                 maps:put({From, Dest}, N + 1, StepsAcc)}
                        end
                end,
            {NQ, NV, NSteps} = lists:foldl(F,
                                           {Q2, Visited, Steps},
                                           maps:get(Valve, PipeMaps, [])),
            steps_impl(NQ, NV, NSteps, From, PipeMaps)
    end.

dfs(Flowrates, Minutes, Current, Pressure, Steps) ->
    F = fun(Valve, Flowrate, Acc) ->
                case Minutes - maps:get({Current, Valve}, Steps) - 1 of
                    Rem when Rem > 0 ->
                        max(Acc, dfs(maps:remove(Valve, Flowrates),
                                     Rem,
                                     Valve,
                                     Pressure + Flowrate * Rem,
                                     Steps));
                    _ ->
                        Acc
                end
        end,
    maps:fold(F, Pressure, Flowrates).
