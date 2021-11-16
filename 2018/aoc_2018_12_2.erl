-module(aoc_2018_12_2).

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
    case io:fread("", "initial state: ~s") of
        eof ->
            eof;
        {ok, [L]} ->
            L2 = lists:zip(L, lists:seq(0, length(L) - 1)),
            F = fun({$#, I}) ->
                        {true, I};
                   ({$., _}) ->
                        false
                end,
            L3 = lists:filtermap(F, L2),
            io:get_line(""),
            G = fun(_, Acc) ->
                        case io:fread("", "~s => ~c") of
                            {ok, [_, [$.]]} ->
                                Acc;
                            {ok, [LLCRR, [$#]]} ->
                                sets:add_element(LLCRR, Acc)
                        end
                end,
            Mapping = lists:foldl(G, sets:new(), lists:seq(1, 32)),
            {hd(L3), lists:last(L3), sets:from_list(L3), Mapping}
    end.

ini() ->
    0.

-define(PATTERN, "##..#......##..#......##..#......##..#......##..#....##..#.......##..#......##..#....##..#.....##..#......##..#......##..#....##..#....##..#").

%% Observe after some iterations the pots exhibit above pattern.
%% Afterwards, each iteration shifts pattern right by one pot.
%% So the increase of sum of pots number per iteration is constant (= #pots).

-define(GENERATIONS, 50000000000).

do({Min, Max, Pots, Mapping}) ->
    validate(Mapping),
    {Generations, Sum} = gen(0, Min, Max, Pots, Mapping),
    L = [case X of $# -> 1; $. -> 0 end || X <- ?PATTERN],
    Sum + (?GENERATIONS - Generations) * lists:sum(L).

validate(Mapping) ->
    Pattern = "...." ++ ?PATTERN ++ "....",
    Next = [case sets:is_element(string:slice(Pattern, I - 1, 5), Mapping) of
                true ->
                    $#;
                false ->
                    $.
            end || I <- lists:seq(1, length(Pattern) - 4)],
    Next = "..." ++ ?PATTERN ++ ".".

gen(Generation, Min, Max, Pots, Mapping) ->
    case pattern(Min, Max, Pots) of
        ?PATTERN ->
            {Generation, lists:sum(sets:to_list(Pots))};
        _ ->
            {NewMin, NewMax, NewPots} = gen_impl(Min, Max, Pots, Mapping),
            gen(Generation + 1, NewMin, NewMax, NewPots, Mapping)
    end.

gen_impl(Min, Max, Pots, Mapping) ->
    Init = {undefined, undefined, sets:new()},
    Range = lists:seq(Min - 2, Max + 2),
    F = fun(I, {MinAcc, MaxAcc, PotsAcc} = Acc) ->
                Key = [case sets:is_element(X, Pots) of
                           true ->
                               $#;
                           false ->
                               $.
                       end || X <- lists:seq(I - 2, I + 2)],
                case sets:is_element(Key, Mapping) of
                    true ->
                        {min(I, MinAcc),
                         case MaxAcc of undefined -> I; _ -> max(I, MaxAcc) end,
                         sets:add_element(I, PotsAcc)};
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, Init, Range).

pattern(Min, Max, Pots) ->
    [case sets:is_element(X, Pots) of
         true ->
             $#;
         false -> $.
     end || X <- lists:seq(Min, Max)].

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
