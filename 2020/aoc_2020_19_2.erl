-module(aoc_2020_19_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

-define(TAB, memo).

run() ->
    ets:new(?TAB, [named_table]),
    Rules = input_rules(),
    {Set42, Set31} = init_sets(Rules),
    run_impl(Set42, Set31, 0).

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

init_sets(Rules) ->
    F = fun(I, {Acc42, Acc31}) ->
                L = gen_string(I),
                New42 = case dp(Rules, L, 42, 1, 8) of
                            true ->
                                sets:add_element(L, Acc42);
                            _ ->
                                Acc42
                        end,
                New31 = case dp(Rules, L, 31, 1, 8) of
                            true ->
                                sets:add_element(L, Acc31);
                            _ ->
                                Acc31
                        end,
                {New42, New31}
        end,
    lists:foldl(F, {sets:new(), sets:new()}, lists:seq(0, 255)).

gen_string(I) ->
    F = fun(_, {X, L}) when X band 1 =:= 0 ->
                {X bsr 1, [$a | L]};
           (_, {X, L}) ->
                {X bsr 1, [$b | L]}
        end,
    {_, Res} = lists:foldl(F, {I, []}, lists:seq(1, 8)),
    Res.

run_impl(Set42, Set31, Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        L ->
            L2 = string:trim(L, trailing, "\n"),
            run_impl(Set42, Set31, Acc + case do(Set42, Set31, L2) of true -> 1; _ -> 0 end)
    end.

%% - Before changing rule, #42 and #31 must match strings of length 8.
%% - Say the set of strings they can match are #42-matched strings and #31-matched strings.
%% - After changing rule, #0 must contain at least N of #42-matched strings as prefix,
%% - and M of #31-matched strings as suffix
%% - where N >= 2 and M >= 1
%% - also, #42-matched intersects #31-matched is empty set.
do(_, _, L) when length(L) rem 8 =/= 0 ->
    false;
do(Set42, Set31, L) ->
    do_impl(Set42, Set31, L, 0, undefined).

do_impl(_, _, [], N42, N31) ->
    N42 >= 2 andalso N31 =/= undefined andalso N31 >= 1 andalso N42 > N31;
do_impl(Set42, Set31, L, N42, undefined) ->
    {S, T} = lists:split(8, L),
    case sets:is_element(S, Set42) of
        true ->
            do_impl(Set42, Set31, T, N42 + 1, undefined);
        _ ->
            do_impl(Set42, Set31, L, N42, 0)
    end;
do_impl(Set42, Set31, L, N42, N31) ->
    {S, T} = lists:split(8, L),
    case sets:is_element(S, Set31) of
        true ->
            do_impl(Set42, Set31, T, N42, N31 + 1);
        _ ->
            false
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
    string:slice(L, Begin - 1, End - Begin + 1).

dp_impl(Rules, L, Rth, Begin, End) ->
    Rule = array:get(Rth, Rules),
    rule(Rules, L, Rule, Begin, End).

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
