-module(aoc_2020_19_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

-define(TAB, memo).
-define(ATOMS, aoc_atoms).
-define(LENGTHS, aoc_lengths).

run() ->
    Rules = input_rules(),
    ets:new(?TAB, [named_table]),
    ets:new(?ATOMS, [named_table]),
    init_lengths(Rules),
    run_impl(Rules, 0).

init_lengths(Rules) ->
    ets:new(?LENGTHS, [named_table]),
    F = fun(I) -> dp_len(Rules, I) end,
    lists:foreach(F, lists:seq(0, array:size(Rules) - 1)).

dp_len(Rules, I) ->
    case ets:lookup(?LENGTHS, I) of
        [{_, Len}] ->
            Len;
        [] ->
            Rule = array:get(I, Rules),
            Len = len(Rules, Rule),
            ets:insert(?LENGTHS, {I, Len}),
            Len
    end.

len(_Rules, a) ->
    1;
len(_Rules, b) ->
    1;
len(Rules, [{R1, R2} | _]) ->
    dp_len(Rules, R1) + dp_len(Rules, R2);
len(Rules, [R | _]) ->
    dp_len(Rules, R).

input_rules() ->
    input_rules_impl(array:new()).

input_rules_impl(A) ->
    case io:get_line("") of
        "\n" ->
            A;
        L ->
            {I, L2} = string:to_integer(L),
            L3 = string:trim(L2, both, ": \n"),
            Parsed =
                case L3 of
                    [$", $a, $"] ->
                        a;
                    [$", $b, $"] ->
                        b;
                    _ ->
                        case string:split(L3, " ", all) of
                            [N1s, N2s, "|", N3s, N4s] ->
                                {N1, _} = string:to_integer(N1s),
                                {N2, _} = string:to_integer(N2s),
                                {N3, _} = string:to_integer(N3s),
                                {N4, _} = string:to_integer(N4s),
                                [{N1, N2}, {N3, N4}];
                            [N1s, "|", N2s] ->
                                {N1, _} = string:to_integer(N1s),
                                {N2, _} = string:to_integer(N2s),
                                [N1, N2];
                            [N1s, N2s] ->
                                {N1, _} = string:to_integer(N1s),
                                {N2, _} = string:to_integer(N2s),
                                [{N1, N2}];
                            [N1s] ->
                                {N1, _} = string:to_integer(N1s),
                                [N1]
                        end
                end,
            input_rules_impl(array:set(I, Parsed, A))
    end.

run_impl(Rules, Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        L ->
            L2 = string:trim(L, trailing, "\n"),
            run_impl(Rules, Acc + do(Rules, L2))
    end.

do(Rules, L) ->
    ets:delete_all_objects(?ATOMS),
    case dp(Rules, L, 0, 1, length(L)) of
        true ->
            1;
        _ ->
            0
    end.


dp(Rules, L, Rth, Begin, End) ->
    Key = key(L, Begin, End),
    case ets:lookup(?TAB, {Rth, Key}) of
        [{_, Bool}] ->
            Bool;
        [] ->
            Res = dp_impl(Rules, L, Rth, Begin, End),
            ets:insert(?TAB, {{Rth, Key}, Res}),
            Res
    end.

key(L, Begin, End) ->
    case ets:lookup(?ATOMS, {Begin, End}) of
        [{_, Atom}] ->
            Atom;
        [] ->
            S = string:slice(L, Begin - 1, End - Begin + 1),
            Key = list_to_atom(S),
            ets:insert(?ATOMS, {{Begin, End}, Key}),
            Key
    end.

dp_impl(Rules, L, Rth, Begin, End) ->
    case ets:lookup_element(?LENGTHS, Rth, 2) of
        Len when Len =/= End - Begin + 1 ->
            false;
        _ ->
            Rule = array:get(Rth, Rules),
            rule(Rules, L, Rule, Begin, End)
    end.

rule(_Rules, L, a, Begin, End) ->
    Begin =:= End andalso lists:nth(Begin, L) =:= $a;
rule(_Rules, L, b, Begin, End) ->
    Begin =:= End andalso lists:nth(Begin, L) =:= $b;
rule(Rules, L, RL, Begin, End) when is_list(RL) ->
    F = fun(Rths) ->
                rule(Rules, L, Rths, Begin, End)
        end,
    lists:any(F, RL);
rule(Rules, L, {R1, R2}, Begin, End) ->
    split(Rules, L, Begin, End, R1, R2);
rule(Rules, L, Rth, Begin, End) when is_integer(Rth) ->
    dp(Rules, L, Rth, Begin, End).

split(_Rules, _L, _Begin, _Begin, _R1, _R2) ->
    false;
split(Rules, L, Begin, End, R1, R2) ->
    F = fun(Mid) ->
                Res1 = dp(Rules, L, R1, Begin, Mid),
                Res2 = dp(Rules, L, R2, Mid + 1, End),
                Res1 andalso Res2
        end,
    lists:any(F, lists:seq(Begin, End - 1)).
