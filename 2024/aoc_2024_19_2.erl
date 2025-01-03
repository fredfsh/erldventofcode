-module(aoc_2024_19_2).

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
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

ini() ->
    L = io:get_line(""),
    {string:lexemes(L, ", \n"), []}.

do(X) ->
    X.

acc({Towels, Acc}, X) ->
    {Towels, [X | Acc]}.

-define(TAB, memo).

fin({Towels, Designs}) ->
    ets:new(?TAB, [named_table]),
    Set = sets:from_list(Towels),
%%    io:format("~p~n", [sets:to_list(Set)]),
    lists:sum([dp(D, Set) || D <- Designs]).

%% Acc: 0 1 2 3 4 5
%% Str: |a|b|c|d|e|
%% Idx:  0 1 2 3 4
dp(Design, Towels) ->
    case ets:lookup_element(?TAB, Design, 2, undefined) of
        undefined ->
            Res = dp_impl(Design, Towels),
            ets:insert_new(?TAB, {Design, Res}),
%%            io:format("dp(~p) = ~p~n", [Design, Res]),
            Res;
        Res ->
            Res
    end.

dp_impl([H | T] = L, Towels) ->
    Init = case sets:is_element(L, Towels) of
               true ->
                   1;
               false ->
                   0
           end,
    F = fun(C, {Acc, PrefixAcc, SuffixAcc}) ->
                NewAcc = case sets:is_element(SuffixAcc, Towels) of
                             true ->
                                 Acc + dp(PrefixAcc, Towels);
                             false ->
                                 Acc
                         end,
%%                io:format("~p + ~p = ~p~n", [PrefixAcc, SuffixAcc, NewAcc]),
                {NewAcc, PrefixAcc ++ [C], tl(SuffixAcc)}
        end,
    {Res, _, _} = lists:foldl(F, {Init, [H], T}, T),
    Res.
