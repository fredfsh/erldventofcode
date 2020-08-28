-module(aoc_2015_11_1).

-export([start/0]).

start() ->
    {ok, [In]} = io:fread("", "~s"),
    Out = do(In),
    io:format("~p~n", [Out]),
    ok.

do(In) ->
    X =
        case init(In) of
            In ->
                inc(In);
            Y ->
                Y
        end,
    do_impl(X).

do_impl(X) ->
    case valid(X) of
        true ->
            X;
        _ ->
            do_impl(inc(X))
    end.

init(X) ->
    init_impl(X, []).

init_impl([], Y) ->
    lists:reverse(Y);
init_impl([X | T], Y) when X =:= $i; X =:= $o; X =:= $l ->
    lists:reverse([X + 1 | Y]) ++ lists:duplicate(length(T), $a);
init_impl([X | T], Y) ->
    init_impl(T, [X | Y]).

valid(X) ->
    valid_impl(X, false, sets:new()).

valid_impl([], false, _) ->
    false;
valid_impl([], true, Set) ->
    sets:size(Set) >= 2;
valid_impl([X, X | T], Rule1, Set) ->
    valid_impl([X | T], Rule1, sets:add_element(X, Set));
valid_impl([X, Y, Z | T], false, Set) when Y =:= X + 1, Z =:= Y + 1 ->
    valid_impl([Z | T], true, Set);
valid_impl([_ | T], Rule1, Set) ->
    valid_impl(T, Rule1, Set).

inc(X) ->
    inc_impl(lists:reverse(X), []).

inc_impl([], Y) ->
    Y;
inc_impl([$z | T], Y) ->
    inc_impl(T, [$a | Y]);
inc_impl([X | T], Y) when X =:= $h; X =:= $n; X =:= $k ->
    lists:reverse([X + 2 | T]) ++ Y;
inc_impl([X | T], Y) ->
    lists:reverse([X + 1 | T]) ++ Y.
