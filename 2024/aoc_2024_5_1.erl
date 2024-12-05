-module(aoc_2024_5_1).

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
        {ok, [L]} ->
            case string:lexemes(L, "|") of
                [A, B] ->
                    {list_to_integer(A), list_to_integer(B)};
                _ ->
                    [list_to_integer(S) || S <- string:lexemes(L, ",")]
            end
    end.

ini() ->
    {[], []}.

do(X) ->
    X.

acc({Rules, Updates}, Rule) when is_tuple(Rule) ->
    {[Rule | Rules], Updates};
acc({Rules, Updates}, Update) when is_list(Update) ->
    {Rules, [Update | Updates]}.

fin({Rules, Updates}) ->
    Order = sets:from_list(Rules),
    G = fun({A, B}) ->
                not sets:is_element({B, A}, Order)
        end,
    F = fun(Update) ->
                lists:all(G, pairs(Update))
        end,
    Corrects = lists:filter(F, Updates),
    Middles = [lists:nth((length(X) + 1) div 2, X) || X <- Corrects],
    lists:sum(Middles).

pairs(L) ->
    pairs_impl([], L).

pairs_impl(Acc, [_]) ->
    Acc;
pairs_impl(Acc, [H | T]) ->
    pairs_impl(lists:append(Acc, [{H, X} || X <- T]), T).
