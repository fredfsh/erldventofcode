-module(aoc_2020_16_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do().

do() ->
    Valid = input_valid(),
    Your = input_your(),
    Invalid = input_nearby(Valid),
    Invalid.

input_valid() ->
    input_valid_impl(sets:new()).

input_valid_impl(Valid) ->
    case io:get_line("") of
        "\n" ->
            Valid;
        L ->
            Parts = string:split(L, " ", all),
            N = length(Parts),
            Valid2 = add_to_valid(lists:nth(N, Parts), Valid),
            input_valid_impl(add_to_valid(lists:nth(N - 2, Parts), Valid2))
    end.

add_to_valid(RangeStr, Valid) ->
    [N1Str, N2Str] = string:split(RangeStr, "-"),
    {N1, _} = string:to_integer(N1Str),
    {N2, _} = string:to_integer(N2Str),
    add_to_valid_impl(Valid, N1, N2).

add_to_valid_impl(Valid, I, End) when I > End ->
    Valid;
add_to_valid_impl(Valid, I, End) ->
    add_to_valid_impl(sets:add_element(I, Valid), I + 1, End).

input_your() ->
    io:get_line(""),
    io:get_line(""),
    io:get_line(""),
    ok.

input_nearby(Valid) ->
    io:get_line(""),
    input_nearby_impl(Valid, 0).

input_nearby_impl(Valid, Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        L ->
            NStrs = string:split(L, ",", all),
            F = fun(NStr, Ac) ->
                        {N, _} = string:to_integer(NStr),
                        case sets:is_element(N, Valid) of
                            true ->
                                Ac;
                            _ ->
                                Ac + N
                        end
                end,
            input_nearby_impl(Valid, lists:foldl(F, Acc, NStrs))
    end.
