-module(aoc_2015_5_2).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(X) ->
    case io:fread("", "~s") of
        {ok, [In]} ->
            case is_nice(In) of
                true ->
                    do_impl(X + 1);
                _ ->
                    do_impl(X)
            end;
        eof ->
            X
    end.

is_nice(X) ->
    is_nice_impl(X, 0, false, false, maps:new()).

is_nice_impl([A, _, A | _], _N, true, false, _Map) ->
    true;
is_nice_impl([A, B, A | T], N, false, false, Map) ->
    case maps:find({A, B}, Map) of
        {ok, M} when M =:= N - 1 ->
            is_nice_impl([B, A | T], N + 1, false, true, Map);
        {ok, _} ->
            true;
        error ->
            Map2 = maps:put({A, B}, N, Map),
            is_nice_impl([B, A | T], N + 1, false, true, Map2)
    end;
is_nice_impl([A, B | T], N, false, Rule2, Map) ->
    case {maps:find({A, B}, Map), Rule2} of
        {{ok, M}, _} when M =:= N - 1 ->
            is_nice_impl([B | T], N + 1, false, Rule2, Map);
        {{ok, _}, true} ->
            true;
        {{ok, _}, false} ->
            is_nice_impl([B | T], N + 1, true, false, Map);
        {error, _} ->
            Map2 = maps:put({A, B}, N, Map),
            is_nice_impl([B | T], N + 1, false, Rule2, Map2)
    end;
is_nice_impl([_ | T], N, Rule1, Rule2, Set) ->
    is_nice_impl(T, N + 1, Rule1, Rule2, Set);
is_nice_impl([], _, _, _, _) ->
    false.
