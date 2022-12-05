-module(aoc_2022_5_2).

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

-define(SPACE, 16#20).

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            case string:trim(L, trailing) of
                "" ->
                    undefined;
                [$m | _] = Line ->
                    [_, NStr, _, FromStr, _, ToStr] =
                        string:split(Line, " ", all),
                    {move,
                     list_to_integer(NStr),
                     list_to_integer(FromStr),
                     list_to_integer(ToStr)};
                [?SPACE, $1 | _] = Line ->
                    Parts = string:split(Line, " ", all),
                    LastStr = lists:last(Parts),
                    {n, list_to_integer(LastStr)};
                Line ->
                    cargos(Line)
            end
    end.

cargos(Line) ->
    cargos_impl([], Line).

cargos_impl(Acc, [A, B, C]) ->
    lists:reverse([cargo(A, B, C) | Acc]);
cargos_impl(Acc, [A, B, C, ?SPACE | T]) ->
    cargos_impl([cargo(A, B, C) | Acc], T).

cargo(?SPACE, ?SPACE, ?SPACE) ->
    undefined;
cargo($[, Cargo, $]) ->
    Cargo.

ini() ->
    [].

do(X) ->
    X.

acc(Cargos, {n, N}) ->
    F = fun(Cargo, Acc) ->
                L = array:to_list(Acc),
                array:from_list(add_cargos(Cargo, L))
        end,
    lists:foldl(F, array:new([N, {default, []}]), Cargos);
acc(Cargos, undefined) ->
    Cargos;
acc(Cargos, {move, N, From, To}) ->
    move(Cargos, N, From, To);
acc(Cargos, Cargo) ->
    [Cargo | Cargos].

add_cargos(Cargos, Stacks) ->
    %%io:format("adding cargos ~p to ~p\n", [Cargos, Stacks]),
    add_cargos_impl([], Cargos, Stacks).

add_cargos_impl(Acc, [], []) ->
    lists:reverse(Acc);
add_cargos_impl(Acc, [], [Stack | T]) ->
    add_cargos_impl([Stack | Acc], [], T);
add_cargos_impl(Acc, [undefined | TC], [Stack | TS]) ->
    add_cargos_impl([Stack | Acc], TC, TS);
add_cargos_impl(Acc, [Cargo | TC], [Stack | TS]) ->
    add_cargos_impl([[Cargo | Stack] | Acc], TC, TS).

move(Cargos, N, From, To) ->
    FromCargos = array:get(From - 1, Cargos),
    {ToMove, Rem} = lists:split(N, FromCargos),
    ToCargos = array:get(To - 1, Cargos),
    StackedCargos = lists:append(ToMove, ToCargos),
    array:set(From - 1, Rem, array:set(To - 1, StackedCargos, Cargos)).

fin(X) ->
    F = fun(_, [H | _], Acc) -> [H | Acc] end,
    lists:reverse(array:foldl(F, [], X)).
