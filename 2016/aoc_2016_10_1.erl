-module(aoc_2016_10_1).

-export([start/0]).

-define(CHIP1, 61).
-define(CHIP2, 17).

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
                  undefined;
              _ ->
                  LowID
          end,
    High = case HighWhat of
               output ->
                   undefined;
               _ ->
                   HighID
           end,
    maps:put(Bot, {Low, High}, Map).

do({BotChips, BotBots}) ->
    do_impl(BotChips, BotBots).

do_impl(BotChips, BotBots) ->
    F = fun(_, _, Acc) when is_integer(Acc) ->
                Acc;
           (Bot, {?CHIP1, ?CHIP2}, _Acc) ->
                Bot;
           (Bot, {?CHIP2, ?CHIP1}, _Acc) ->
                Bot;
           (Bot, {Chip1, Chip2}, Acc) when Chip1 < Chip2 ->
                dispatch(Bot, Chip1, Chip2, BotBots, Acc);
           (Bot, {Chip1, Chip2}, Acc) ->
                dispatch(Bot, Chip2, Chip1, BotBots, Acc);
           (Bot, Chip, Acc) ->
                assign(Chip, Bot, Acc)
        end,
    case maps:fold(F, maps:new(), BotChips) of
        Bot when is_integer(Bot) ->
            Bot;
        NewBotChips ->
            do_impl(NewBotChips, BotBots)
    end.

dispatch(Bot, LowChip, HighChip, BotBots, BotChips) ->
    case maps:get(Bot, BotBots) of
        {undefined, undefined} ->
            BotChips;
        {undefined, High} ->
            assign(HighChip, High, BotChips);
        {Low, undefined} ->
            assign(LowChip, Low, BotChips);
        {Low, High} ->
            assign(HighChip, High, assign(LowChip, Low, BotChips))
    end.
