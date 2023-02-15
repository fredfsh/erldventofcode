-module(aoc_2022_21_1).

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
                [ID1, OpStr, ID2] ->
                    {ID, {op(OpStr), list_to_atom(ID1), list_to_atom(ID2)}}
            end
    end.

op("+") -> fun(X, Y) -> X + Y end;
op("-") -> fun(X, Y) -> X - Y end;
op("*") -> fun(X, Y) -> X * Y end;
op("/") -> fun(X, Y) -> X div Y end.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, {X, Y}) ->
    maps:put(X, Y, Acc).

fin(X) ->
    {Res, _} = yell(root, X),
    Res.

yell(Name, Monkeys) ->
    case maps:get(Name, Monkeys) of
        N when is_integer(N) ->
            {N, Monkeys};
        {Fun, Left, Right} ->
            {L, M1} = yell(Left, Monkeys),
            {R, M2} = yell(Right, M1),
            Val = Fun(L, R),
            {Val, maps:put(Name, Val, M2)}
    end.
