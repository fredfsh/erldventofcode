-module(aoc_2016_10_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(maps:new(), maps:new()).

input_impl(BotChips, BotBots) ->
    case io:fread("", "~s") of
        eof ->
            {BotChips, BotBots};
        {ok, ["value"]} ->
            {ok, [Chip, Bot]} = io:fread("", "~d goes to bot ~d"),
            input_impl(assign(Chip, Bot, BotChips), BotBots);
        {ok, ["bot"]} ->
            {ok, [Bot, LowWhat, LowID, HighWhat, HighID]}
                = io:fread("", "~d gives low to ~a ~d and high to ~a ~d"),
            input_impl(BotChips,
                       instruct(Bot, LowWhat, LowID, HighWhat, HighID, BotBots))
    end.

assign(Chip, Bot, Map) ->
    maps:update_with(Bot, fun(Val) -> {Val, Chip} end, Chip, Map).

instruct(Bot, LowWhat, LowID, HighWhat, HighID, Map) ->
    Low = case LowWhat of
              output ->
                  {output, LowID};
              _ ->
                  LowID
          end,
    High = case HighWhat of
               output ->
                   {output, HighID};
               _ ->
                   HighID
           end,
    maps:put(Bot, {Low, High}, Map).

do({BotChips, BotBots}) ->
    do_impl(BotChips, BotBots, maps:new()).

do_impl(BotChips, BotBots, OutputChips) ->
    case maps:size(BotChips) of
        0 ->
            maps:get(0, OutputChips)
                * maps:get(1, OutputChips)
                * maps:get(2, OutputChips);
        _ ->
            F = fun(Bot, {Chip1, Chip2}, Acc) when Chip1 < Chip2 ->
                        dispatch(Bot, Chip1, Chip2, BotBots, Acc);
                   (Bot, {Chip1, Chip2}, Acc) ->
                        dispatch(Bot, Chip2, Chip1, BotBots, Acc);
                   (Bot, Chip, {BotChipsAcc, OutputChipsAcc}) ->
                        {assign(Chip, Bot, BotChipsAcc), OutputChipsAcc}
                end,
            {NewBotChips, NewOutputChips}
                = maps:fold(F, {maps:new(), OutputChips}, BotChips),
            do_impl(NewBotChips, BotBots, NewOutputChips)
    end.

dispatch(Bot, LowChip, HighChip, BotBots, {BotChips, OutputChips}) ->
    case maps:get(Bot, BotBots) of
        {{output, LowID}, {output, HighID}} ->
            {BotChips,
             maps:put(LowID, LowChip, maps:put(HighID, HighChip, OutputChips))};
        {{output, LowID}, High} ->
            {assign(HighChip, High, BotChips),
             maps:put(LowID, LowChip, OutputChips)};
        {Low, {output, HighID}} ->
            {assign(LowChip, Low, BotChips),
             maps:put(HighID, HighChip, OutputChips)};
        {Low, High} ->
            {assign(HighChip, High, assign(LowChip, Low, BotChips)),
             OutputChips}
    end.
