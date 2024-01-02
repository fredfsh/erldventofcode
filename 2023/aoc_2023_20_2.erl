-module(aoc_2023_20_2).

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
    case io:fread("", "~s ->") of
        eof ->
            eof;
        {ok, [X]} ->
            L = io:get_line(""),
            Outputs = parse_outputs(string:trim(L)),
            parse_module(X, Outputs)
    end.

parse_outputs(S) ->
    Parts = string:split(S, ",", all),
    [list_to_atom(string:trim(P)) || P <- Parts].

-define(BROADCASTER, broadcaster).
-record(?BROADCASTER, {outputs}).
-record(flipflop, {outputs = [], state = off}).
-record(conjunction, {outputs, inputs = maps:new(), highs = 0}).

parse_module("broadcaster", Outputs) ->
    #?BROADCASTER{outputs = Outputs};
parse_module([$% | T], Outputs) ->
    {list_to_atom(T), #flipflop{outputs = Outputs}};
parse_module([$& | T], Outputs) ->
    {list_to_atom(T), #conjunction{outputs = Outputs}}.

ini() ->
    {undefined, maps:new()}.

do(X) ->
    X.

acc({undefined, Modules}, #?BROADCASTER{} = Broadcaster) ->
    {Broadcaster, Modules};
acc({Broadcaster, Modules}, {Name, Module}) ->
    {Broadcaster, maps:put(Name, Module, Modules)}.

%% Observe `rx` has a single conjunction input `ls`.
%% `ls` has four conjunction inputs: tx, dd, nz, ph.
%% If they all send high pulses to `ls`, `ls` would send a low pulse to `rx`.
%% Suspect they send high pulses in cycles.
%% Verify then multiple them together.

-define(MODULES, [tx, dd, nz, ph]).

fin({Broadcaster, Modules}) ->
    Init = process_inputs(Modules, Broadcaster),
    F = fun(Module, Acc) -> Acc * cycle(Init, Module, Broadcaster) end,
    lists:foldl(F, 1, ?MODULES).

process_inputs(Modules, #?BROADCASTER{outputs = BO}) ->
    F = fun(Name, Module, Acc) ->
                Outputs = element(2, Module),
                add_input(Name, Outputs, Acc)
        end,
    maps:fold(F, add_input(?BROADCASTER, BO, Modules), Modules).

add_input(Input, Outputs, Modules) ->
    F = fun(Output, Acc) ->
                case maps:get(Output, Acc, undefined) of
                    undefined ->
                        %% use a dummy flip-flop for untyped module
                        maps:put(Output, #flipflop{}, Acc);
                    #conjunction{inputs = Inputs} = Conjunction ->
                        NewInputs = maps:put(Input, low, Inputs),
                        Module = Conjunction#conjunction{inputs = NewInputs},
                        maps:put(Output, Module, Acc);
                    _ ->
                        Acc
                end
        end,
    lists:foldl(F, Modules, Outputs).

cycle(Modules, Target, Broadcaster) ->
    cycle_impl(undefined, 1, Modules, Target, Broadcaster).

cycle_impl(History, Acc, Modules, Target, Broadcaster) ->
    %%io:format("Cycle #~p:~n", [Acc]),
    {NewModules, Highs} = push_button(Modules, Target, Broadcaster),
    case {Highs, History} of
        {1, undefined} ->
            cycle_impl(Acc, Acc + 1, NewModules, Target, Broadcaster);
        {1, {N, M}} ->
            C = M - N,
            C = Acc - M,
            C;
        {1, N} ->
            cycle_impl({N, Acc}, Acc + 1, NewModules, Target, Broadcaster);
        _ ->
            cycle_impl(History, Acc + 1, NewModules, Target, Broadcaster)
    end.

push_button(Modules, Target, #?BROADCASTER{outputs = Outputs}) ->
    Q = send_pulses(queue:new(), ?BROADCASTER, Outputs, low),
    push_button_impl(0, Q, Modules, Target).

send_pulses(Q, Input, Outputs, Pulse) ->
    F = fun(Output, Acc) ->
                queue:in({Input, Output, Pulse}, Acc)
        end,
    lists:foldl(F, Q, Outputs).

push_button_impl(Acc, Q, Modules, Target) ->
    case queue:out(Q) of
        {empty, _} ->
            {Modules, Acc};
        {{value, {Input, Output, Pulse}}, Q2} ->
            %%Output =:= ls andalso io:format("~p -> ~p (~p)~n", [Input, Output, Pulse]),
            {NewQ, NewModules} =
                case maps:get(Output, Modules) of
                    #flipflop{} when Pulse =:= high ->
                        {Q2, Modules};
                    #flipflop{outputs = Outputs} = Flipflop when Pulse =:= low ->
                        {Module, NewPulse} = flip(Flipflop),
                        {send_pulses(Q2, Output, Outputs, NewPulse),
                         maps:put(Output, Module, Modules)};
                    #conjunction{outputs = Outputs} = Conjunction ->
                        {Module, NewPulse} = remember(Conjunction, Input, Pulse),
                        {send_pulses(Q2, Output, Outputs, NewPulse),
                         maps:put(Output, Module, Modules)}
                end,
            NewAcc = case {Input, Output, Pulse} of
                         {Target, ls, high} ->
                             Acc + 1;
                         _ ->
                             Acc
                     end,
            push_button_impl(NewAcc, NewQ, NewModules, Target)
    end.

flip(#flipflop{state = off} = Flipflop) ->
    {Flipflop#flipflop{state = on}, high};
flip(#flipflop{state = on} = Flipflop) ->
    {Flipflop#flipflop{state = off}, low}.

remember(#conjunction{inputs=Inputs, highs=Highs} = Conjunction, Input, Pulse) ->
    NewHighs = case {maps:get(Input, Inputs), Pulse} of
                   {X, X} ->
                       Highs;
                   {high, low} ->
                       Highs - 1;
                   {low, high} ->
                       Highs + 1
               end,
    NewPulse = case map_size(Inputs) of
                   NewHighs ->
                       low;
                   _ ->
                       high
               end,
    {Conjunction#conjunction{inputs = Inputs#{Input := Pulse}, highs = NewHighs},
     NewPulse}.
