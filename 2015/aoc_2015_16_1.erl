-module(aoc_2015_16_1).

-export([start/0]).

start() ->
    In = input(),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

input() ->
    input_impl([]).

input_impl(PL) ->
    case io:fread("", "Sue ~d:") of
        eof ->
            PL;
        {ok, [N]} ->
            Map = read_traits(),
            input_impl([{N, Map} | PL])
    end.

read_traits() ->
    L = io:get_line(""),
    Parts = string:split(L, ",", all),
    F = fun(Part, Map) ->
                [KeyStr, ValStr] = string:split(Part, ":"),
                Key = string:trim(KeyStr, leading),
                Val = list_to_integer(string:trim(ValStr)),
                maps:put(Key, Val, Map)
        end,
    lists:foldl(F, maps:new(), Parts).

do(PL) ->
    [{N, _}] = lists:filter(fun({_, M}) -> match(M) end, PL),
    N.

-define(TRAITS,
        [{"children", 3},
         {"cats", 7},
         {"samoyeds", 2},
         {"pomeranians", 3},
         {"akitas", 0},
         {"vizslas", 0},
         {"goldfish", 5},
         {"trees", 3},
         {"cars", 2},
         {"perfumes", 1}]).

match(Map) ->
    F = fun({Key, Val}) ->
                maps:get(Key, Map, Val) =:= Val
        end,
    lists:all(F, ?TRAITS).
