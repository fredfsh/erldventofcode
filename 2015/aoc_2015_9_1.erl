-module(aoc_2015_9_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    {Keys, Map} = input(),
    [X | _] = Keys,
    Set = sets:new(),
    Visited = sets:add_element(X, Set),
    dfs(X, 1, Visited, _Acc = 0, _MaxHop = 0, _First = X, Keys, Map).

input() ->
    input_impl(sets:new(), maps:new()).

input_impl(S, M) ->
    case io:get_line("") of
        eof ->
            {sets:to_list(S), M};
        L ->
            L2 = string:trim(L, trailing, "\n"),
            [As, "to", Bs, "=", Ds] = string:split(L2, " ", all),
            A = list_to_atom(As),
            S2 = sets:add_element(A, S),
            B = list_to_atom(Bs),
            S3 = sets:add_element(B, S2),
            D = list_to_integer(Ds),
            M2 = maps:put({A, B}, D, M),
            M3 = maps:put({B, A}, D, M2),
            input_impl(S3, M3)
    end.

dfs(X, N, _Visited, Acc, MaxHop, First, Keys, Map) when N =:= length(Keys) ->
    case maps:get({X, First}, Map) of
        D when D > MaxHop ->
            Acc;
        E ->
            Acc + E - MaxHop
    end;
dfs(X, N, Visited, DistAcc, MaxHop, First, Keys, Map) ->
    F = fun(Y, Acc) ->
                case sets:is_element(Y, Visited) of
                    true ->
                        Acc;
                    _ ->
                        L = maps:get({X, Y}, Map),
                        D = dfs(Y,
                                N + 1,
                                sets:add_element(Y, Visited),
                                DistAcc + L,
                                max(MaxHop, L),
                                First,
                                Keys,
                                Map),
                        min(Acc, D)
                end
        end,
    lists:foldl(F, undefined, Keys).
