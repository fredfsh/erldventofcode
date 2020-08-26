-module(aoc_2015_7_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    Map = maps:new(),
    Map2 = input(Map),
    {X, _} = dfs(a, Map2),
    X.

input(M) ->
    case io:get_line("") of
        eof ->
            M;
        L ->
            L2 = string:trim(L, trailing),
            {Outs, Exp} =
                case string:split(L2, " ", all) of
                    [Is, "->", Os] ->
                        {Os, to_in(Is)};
                    ["NOT", Is, "->", Os] ->
                        {Os, {'bnot', to_in(Is)}};
                    [I1s, "AND", I2s, "->", Os] ->
                        {Os, {'band', to_in(I1s), to_in(I2s)}};
                    [I1s, "OR", I2s, "->", Os] ->
                        {Os, {'bor', to_in(I1s), to_in(I2s)}};
                    [I1s, "LSHIFT", I2s, "->", Os] ->
                        {Os, {'bsl', to_in(I1s), list_to_integer(I2s)}};
                    [I1s, "RSHIFT", I2s, "->", Os] ->
                        {Os, {'bsr', to_in(I1s), list_to_integer(I2s)}}
                end,
            M2 = maps:put(list_to_atom(Outs), Exp, M),
            input(M2)
    end.

to_in(S) ->
    try
        list_to_integer(S)
    catch
        _:_ ->
            list_to_atom(S)
    end.

dfs(X, M) when is_integer(X) ->
    {X, M};
dfs(X, M) ->
    case maps:get(X, M) of
        Y when not is_tuple(Y) ->
            dfs(Y, M);
        {'bnot', Y} ->
            {V, M2} = dfs(Y, M),
            Res = (bnot V) band 16#FFFF,
            {Res, maps:put(X, Res, M2)};
        {'band', Y, Z} ->
            {V1, M2} = dfs(Y, M),
            {V2, M3} = dfs(Z, M2),
            Res = V1 band V2,
            {Res, maps:put(X, Res, M3)};
        {'bor', Y, Z} ->
            {V1, M2} = dfs(Y, M),
            {V2, M3} = dfs(Z, M2),
            Res = V1 bor V2,
            {Res, maps:put(X, Res, M3)};
        {'bsl', Y, N} ->
            {V, M2} = dfs(Y, M),
            Res = (V bsl N) band 16#FFFF,
            {Res, maps:put(X, Res, M2)};
        {'bsr', Y, N} ->
            {V, M2} = dfs(Y, M),
            Res = V bsr N,
            {Res, maps:put(X, Res, M2)}
    end.
