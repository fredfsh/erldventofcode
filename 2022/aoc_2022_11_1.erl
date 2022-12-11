-module(aoc_2022_11_1).

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

-record(monkey, {id, items, op, test, inspection = 0}).

input() ->
    case io:fread("", "Monkey ~d:") of
        eof ->
            eof;
        {ok, [ID]} ->
            Line1 = io:get_line(""),
            [_, _ | Parts1] = string:split(string:trim(Line1), " ", all),
            Items = [list_to_integer(string:trim(X, trailing, ","))
                     || X <- Parts1],

            Op = case io:fread("", " Operation: new = old ~c ~s") of
                     {ok, ["*", "old"]} ->
                         fun(X) -> X * X end;
                     {ok, ["*", NStr]} ->
                         N = list_to_integer(NStr),
                         fun(X) -> X * N end;
                     {ok, ["+", NStr]} ->
                         N = list_to_integer(NStr),
                         fun(X) -> X + N end
                 end,

            {ok, [Div]} = io:fread("", " Test: divisible by ~d"),
            {ok, [True]} = io:fread("", " If true: throw to monkey ~d"),
            {ok, [False]} = io:fread("", " If false: throw to monkey ~d"),

            io:get_line(""),

            #monkey{id = ID, items = Items, op = Op, test = {Div, True, False}}
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-define(ROUNDS, 20).

fin(X) ->
    sim(?ROUNDS, X).

sim(Monkeys) ->
    F = fun(I, Acc) ->
                Monkey = array:get(I, Acc),
                #monkey{items = Items, op = Op, test = {Div, True, False},
                        inspection = Inspection} = Monkey,
                #monkey{items = TItems} = TMonkey = array:get(True, Acc),
                #monkey{items = FItems} = FMonkey = array:get(False, Acc),

                G = fun(Item, {TAcc, FAcc}) ->
                            NItem = Op(Item) div 3,
                            case NItem rem Div of
                                0 ->
                                    {TAcc ++ [NItem], FAcc};
                                _ ->
                                    {TAcc, FAcc ++ [NItem]}
                            end
                    end,
                {NTItems, NFItems} = lists:foldl(G, {TItems, FItems}, Items),

                NMonkey = Monkey#monkey{items = [],
                                        inspection = Inspection + length(Items)},
                Acc2 = array:set(I, NMonkey, Acc),
                Acc3 = array:set(True, TMonkey#monkey{items = NTItems}, Acc2),
                Acc4 = array:set(False, FMonkey#monkey{items = NFItems}, Acc3),
                Acc4
        end,
    lists:foldl(F, Monkeys, lists:seq(0, array:size(Monkeys) - 1)).

sim(0, Monkeys) ->
    business(Monkeys);
sim(N, Monkeys) ->
    sim(N - 1, sim(Monkeys)).


business(Monkeys) ->
    %%    io:format("~p", [Monkeys]),
    F = fun(_, #monkey{inspection = Inspection}) -> Inspection end,
    Inspections = array:to_list(array:map(F, Monkeys)),
    Sorted = lists:sort(Inspections),
    lists:last(Sorted) * lists:nth(length(Sorted) - 1, Sorted).
