-module(aoc_2021_24_1).

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
    case io:fread("", "~a ~a") of
        eof ->
            eof;
        {ok, [inp, R]} ->
            {inp, R};
        {ok, [Op, R]} ->
            {ok, [S]} = io:fread("", " ~s"),
            case string:to_integer(S) of
                {error, _} ->
                    {Op, R, list_to_atom(S)};
                {N, _} ->
                    {Op, R, N}
            end
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(Program) ->
    Functions = breakup(lists:reverse(Program)),
    ExprConds = analyze(Functions),
    solve(ExprConds).

breakup([{inp, w} | T]) ->
    breakup_impl([], [{inp, w}], T).

breakup_impl(Funs, Fun, []) ->
    lists:reverse([lists:reverse(Fun) | Funs]);
breakup_impl(Funs, Fun, [{inp, w} | T]) ->
    breakup_impl([lists:reverse(Fun) | Funs], [{inp, w}], T);
breakup_impl(Funs, Fun, [H | T]) ->
    breakup_impl(Funs, [H | Fun], T).

analyze(Functions) ->
    F = fun([{inp, w} | T], {I, Acc}) ->
                G = fun(Map) -> Map#{w => {i, I}} end,
                {I + 1, analyze_impl(T, lists:map(G, Acc))}
        end,
    Init = [#{z => [], ifs => []}],
    {_, ExprConds} = lists:foldl(F, {0, Init}, Functions),
    ExprConds.

analyze_impl([], ExprConds) ->
    F = fun(#{z := Z, ifs := Cond}) -> #{z => Z, ifs => Cond} end,
    lists:map(F, ExprConds);
analyze_impl([{mul, x, 0} | T], ExprConds) ->
    F = fun(Map) -> Map#{x => 0} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{add, x, z} | T], ExprConds) ->
    F = fun(#{x := 0, z := Z} = Map) -> Map#{x => Z} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{mod, x, 26} | T], ExprConds) ->
    F = fun(#{x := X} = Map) -> Map#{x => mod26(X)} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{'div', z, 1} | T], ExprConds) ->
    analyze_impl(T, ExprConds);
analyze_impl([{'div', z, 26} | T], ExprConds) ->
    F = fun(#{z := Z} = Map) -> Map#{z => div26(Z)} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{add, x, N} | T], ExprConds) ->
    F = fun(#{x := {{i, I}, C}} = Map) ->
                Map#{x => {{i, I}, C + N}};
           (#{x := []} = Map) ->
                Map#{x => N}
        end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{eql, x, w} | T], ExprConds) ->
    F = fun(#{x := {{i, _}, N}} = Map) when N > 9 ->
                [Map#{x => 0}];
           (#{x := N} = Map) when is_integer(N), N > 9 ->
                [Map#{x => 0}];
           (#{x := X, w := W, ifs := Cond} = Map) ->
                [Map#{x => 0, ifs => [{'=/=', X, W} | Cond]},
                 Map#{x => 1, ifs => [{'=:=', X, W} | Cond]}]
        end,
    analyze_impl(T, lists:flatmap(F, ExprConds));
analyze_impl([{eql, x, 0} | T], ExprConds) ->
    F = fun(#{x := X} = Map) -> Map#{x => 1 - X} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{mul, y, 0} | T], ExprConds) ->
    F = fun(Map) -> Map#{y => 0} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{add, y, 25} | T], ExprConds) ->
    F = fun(#{y := 0} = Map) -> Map#{y => 25} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{mul, y, x} | T], ExprConds) ->
    F = fun(#{y := Y, x := 1} = Map) ->
                Map#{y => Y};
           (#{x := 0} = Map) ->
                Map#{y => 0}
        end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{add, y, 1} | T], ExprConds) ->
    F = fun(#{y := Y} = Map) -> Map#{y => Y + 1} end,
    analyze_impl(T, lists:map(F, ExprConds));
%% this will be handled by {add, z, y} below regardless of y = 1 or y = 26
analyze_impl([{mul, z, y} | T], ExprConds) ->
    analyze_impl(T, ExprConds);
analyze_impl([{add, y, w} | T], ExprConds) ->
    F = fun(#{y := 0, w := W} = Map) -> Map#{y => W} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{add, y, N} | T], ExprConds) ->
    F = fun(#{y := Y} = Map) -> Map#{y => {Y, N}} end,
    analyze_impl(T, lists:map(F, ExprConds));
analyze_impl([{add, z, y} | T], ExprConds) ->
    F = fun(#{y := 0} = Map) ->
                Map;
           (#{z := Z, y := Y} = Map) ->
                Map#{z => Z ++ [Y]}
        end,
    analyze_impl(T, lists:map(F, ExprConds)).

mod26([]) ->
    [];
mod26(L) ->
    lists:last(L).

div26([]) ->
    [];
div26(L) ->
    lists:droplast(L).

%% Observe only one condition leads to z = 0:
%%   it consists of 7 equations without overlapping on any input digit :)
solve(ExprConds) ->
    F = fun(#{z := [], ifs := Conds}) ->
                {true, Conds};
           (_) ->
                false
        end,
    [Conds] = lists:filtermap(F, ExprConds),
    G = fun({'=:=', {{i, I}, N}, {i, J}}, Acc) when N > 0 ->
                array:set(I, 9 - N, array:set(J, 9, Acc));
           ({'=:=', {{i, I}, N}, {i, J}}, Acc) when N < 0 ->
                array:set(I, 9, array:set(J, 9 + N, Acc))
        end,
    Arr = lists:foldl(G, array:new(), Conds),
    H = fun(_, V, Acc) ->
                Acc * 10 + V
        end,
    array:foldl(H, 0, Arr).
