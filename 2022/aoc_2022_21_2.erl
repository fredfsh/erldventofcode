-module(aoc_2022_21_2).

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
        {ok, [IDStr]} ->
            ID = list_to_atom(string:trim(IDStr, trailing, ":")),
            Expr = string:trim(io:get_line("")),
            case string:split(Expr, " ", all) of
                [NStr] ->
                    {ID, list_to_integer(NStr)};
                [ID1, Op, ID2] ->
                    {ID, {list_to_atom(ID1), Op, list_to_atom(ID2)}}
            end
    end.

ini() ->
    {maps:new(), maps:new(), 0, maps:new()}.

do(X) ->
    X.

-define(ZERO, zero).
-define(VAR, humn).
-record(expr, {res, left, op, right}).

acc(Acc, {?VAR, _}) ->
    Acc;
acc({Vars, VarExprs, ID, Exprs}, {root, {X, _, Y}}) ->
    F = fun(Set) -> sets:add_element(ID, Set) end,
    VE1 = maps:update_with(X, F, sets:from_list([ID]), VarExprs),
    VE2 = maps:update_with(Y, F, sets:from_list([ID]), VE1),
    VE3 = maps:put(?ZERO, sets:new(), VE2),
    Expr = #expr{res = X, left = Y, op = "+", right = ?ZERO},
    {maps:put(?ZERO, 0, Vars), VE3, ID + 1, maps:put(ID, Expr, Exprs)};
acc({Vars, VarExprs, ID, Exprs}, {Z, {X, Op, Y}}) ->
    F = fun(Set) -> sets:add_element(ID, Set) end,
    VE1 = maps:update_with(X, F, sets:from_list([ID]), VarExprs),
    VE2 = maps:update_with(Y, F, sets:from_list([ID]), VE1),
    VE3 = maps:update_with(Z, F, sets:from_list([ID]), VE2),
    Expr = #expr{res = Z, left = X, op = Op, right = Y},
    {Vars, VE3, ID + 1, maps:put(ID, Expr, Exprs)};
acc({Vars, VarExprs, ID, Exprs}, {Var, Val}) when is_integer(Val) ->
    {maps:put(Var, Val, Vars), VarExprs, ID, Exprs}.

fin({Vars, VarExprs, _, Exprs}) ->
    yell(Vars, VarExprs, Exprs).

yell(Vars, VarExprs, Exprs) ->
    Q = queue:from_list(maps:keys(Vars)),
    bfs(Q, Vars, VarExprs, Exprs).

bfs(Q, Vars, VarExprs, Exprs) ->
    {{value, Var}, Q2} = queue:out(Q),
    F = fun(ID, {QAcc, VarsAcc} = Acc) ->
                case solve(maps:get(ID, Exprs), Vars) of
                    {?VAR, Res} ->
                        Res;
                    {V, Val} ->
                        {queue:in(V, QAcc), maps:put(V, Val, VarsAcc)};
                    undefined ->
                        Acc
                end;
           (_, Res) when is_integer(Res) ->
                Res
        end,
    case sets:fold(F, {Q2, Vars}, maps:get(Var, VarExprs)) of
        Res when is_integer(Res) ->
            Res;
        {NewQ, NewVars} ->
            bfs(NewQ, NewVars, VarExprs, Exprs)
    end.

solve(#expr{res = Z, left = X, op = Op, right = Y}, Map) ->
    case {maps:get(Z, Map, u), maps:get(X, Map, u), Op, maps:get(Y, Map, u)} of
        {u, L, "+", R} when is_integer(L), is_integer(R) ->
            {Z, L + R};
        {L, u, "+", R} when is_integer(L), is_integer(R) ->
            {X, L - R};
        {L, R, "+", u} when is_integer(L), is_integer(R) ->
            {Y, L - R};
        {u, L, "-", R} when is_integer(L), is_integer(R) ->
            {Z, L - R};
        {L, u, "-", R} when is_integer(L), is_integer(R) ->
            {X, L + R};
        {L, R, "-", u} when is_integer(L), is_integer(R) ->
            {Y, R - L};
        {u, L, "*", R} when is_integer(L), is_integer(R) ->
            {Z, L * R};
        {L, u, "*", R} when is_integer(L), is_integer(R) ->
            {X, L div R};
        {L, R, "*", u} when is_integer(L), is_integer(R) ->
            {Y, L div R};
        {u, L, "/", R} when is_integer(L), is_integer(R) ->
            {Z, L div R};
        {L, u, "/", R} when is_integer(L), is_integer(R) ->
            {X, L * R};
        {L, R, "/", u} when is_integer(L), is_integer(R) ->
            {Y, R div L};
        _ ->
            undefined
    end.
