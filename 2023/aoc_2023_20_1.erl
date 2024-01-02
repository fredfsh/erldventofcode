-module(aoc_2023_20_1).

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

-define(TIMES, 1000).

fin({Broadcaster, Modules}) ->
    Init = process_inputs(Modules, Broadcaster),
    push_buttons(0, 0, ?TIMES, Init, Broadcaster).

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

push_buttons(Lows, Highs, 0, _, _) ->
    Lows * Highs;
push_buttons(Lows, Highs, Times, Modules, Broadcaster) ->
    {L, H, NewModules} = push_button(Modules, Broadcaster),
    push_buttons(Lows + L, Highs + H, Times - 1, NewModules, Broadcaster).

push_button(Modules, #?BROADCASTER{outputs = Outputs}) ->
    Q = send_pulses(queue:new(), ?BROADCASTER, Outputs, low),
    push_button_impl(1, 0, Q, Modules).

send_pulses(Q, Input, Outputs, Pulse) ->
    F = fun(Output, Acc) ->
                queue:in({Input, Output, Pulse}, Acc)
        end,
    lists:foldl(F, Q, Outputs).

push_button_impl(Lows, Highs, Q, Modules) ->
    case queue:out(Q) of
        {empty, _} ->
            {Lows, Highs, Modules};
        {{value, {Input, Output, Pulse}}, Q2} ->
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
            {NewLows, NewHighs} = case Pulse of
                                      low ->
                                          {Lows + 1, Highs};
                                      high ->
                                          {Lows, Highs + 1}
                                  end,
            push_button_impl(NewLows, NewHighs, NewQ, NewModules)
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
