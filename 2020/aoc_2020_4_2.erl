-module(aoc_2020_4_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        [] ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    input_impl([]).

input_impl(Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        "\n" ->
            Acc;
        X ->
            input_impl([string:trim(X, trailing) | Acc])
    end.

do(X) ->
    do_impl(X, []).

-define(REQUIRED, ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]).

do_impl([], KVs) ->
    case validate(KVs) of
        true ->
            1;
        _ ->
            0
    end;
do_impl([L | T], KVs) ->
    do_impl(T, add(L, KVs)).

add(L, KVs) ->
    add_impl(L, KVs, [], undefined).

-define(SPACE, 16#20).

add_impl([], KVs, Key, Val) ->
    [{lists:reverse(Key), lists:reverse(Val)} | KVs];
add_impl([$: | T], KVs, Key, undefined) ->
    add_impl(T, KVs, Key, []);
add_impl([?SPACE | T], KVs, Key, Val) ->
    add_impl(T, [{lists:reverse(Key), lists:reverse(Val)} | KVs], [], undefined);
add_impl([X | T], KVs, Key, undefined) when is_list(Key) ->
    add_impl(T, KVs, [X | Key], undefined);
add_impl([X | T], KVs, Key, Val) when is_list(Val) ->
    add_impl(T, KVs, Key, [X | Val]).

validate(KVs) ->
    F = fun(Field) ->
                case proplists:get_value(Field, KVs, undefined) of
                    undefined ->
                        false;
                    Val ->
                        validate(Field, Val)
                end
        end,
    lists:all(F, ?REQUIRED).

validate("byr", Val) ->
    case string:to_integer(Val) of
        {N, []} when N >= 1920, N =< 2002 ->
            true;
        _ ->
            false
    end;

validate("iyr", Val) ->
    case string:to_integer(Val) of
        {N, []} when N >= 2010, N =< 2020 ->
            true;
        _ ->
            false
    end;

validate("eyr", Val) ->
    case string:to_integer(Val) of
        {N, []} when N >= 2020, N =< 2030 ->
            true;
        _ ->
            false
    end;

validate("hgt", Val) ->
    case string:to_integer(Val) of
        {N, "cm"} when N >= 150, N =< 193 ->
            true;
        {N, "in"} when N >= 59, N =< 76 ->
            true;
        _ ->
            false
    end;

validate("hcl", [$#, A, B, C, D, E, F]) ->
    Fun = fun(X) when X >= $0, X =< $9 -> true;
             (X) when X >= $a, X =< $f -> true;
             (_) -> false
          end,
    lists:all(Fun, [A, B, C, D, E, F]);
validate("hcl", _) ->
    false;

validate("ecl", "amb") -> true;
validate("ecl", "blu") -> true;
validate("ecl", "brn") -> true;
validate("ecl", "gry") -> true;
validate("ecl", "grn") -> true;
validate("ecl", "hzl") -> true;
validate("ecl", "oth") -> true;
validate("ecl", _) -> false;

validate("pid", L) when length(L) =:= 9 ->
    F = fun(X) when X >= $0, X =< $9 -> true;
           (_) -> false
        end,
    lists:all(F, L);
validate("pid", _) ->
    false.
