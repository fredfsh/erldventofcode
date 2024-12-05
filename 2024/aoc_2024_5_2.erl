-module(aoc_2024_5_2).

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
                sets:is_element({B, A}, Order)
        end,
    F = fun(Update) ->
                lists:any(G, pairs(Update))
        end,
    Incorrects = lists:filter(F, Updates),
    Corrects = [toposort(X, Order) || X <- Incorrects],
    Middles = [lists:nth((length(X) + 1) div 2, X) || X <- Corrects],
    lists:sum(Middles).

pairs(L) ->
    pairs_impl([], L).

pairs_impl(Acc, [_]) ->
    Acc;
pairs_impl(Acc, [H | T]) ->
    pairs_impl(lists:append(Acc, [{H, X} || X <- T]), T).

toposort(L, Order) ->
    toposort_impl([], L, Order).

toposort_impl(Acc, [], _) ->
    Acc;
toposort_impl(Acc, L, Order) ->
    L1 = [{X, lists:delete(X, L)} || X <- L],
    L2 = [{X, length([undefined || Z <- Y, sets:is_element({Z, X}, Order)])}
          || {X, Y} <- L1],
    L3 = [X || {X, 0} <- L2],
    toposort_impl(Acc ++ L3, lists:foldl(fun lists:delete/2, L, L3), Order).
