-module(aoc_2022_13_1).

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
    case io:fread("", "~s ~s") of
        eof ->
            eof;
        {ok, [Left, Right]} ->
            io:get_line(""),
            {Left, Right}
    end.

ini() ->
    {1, 0}.

do({Left, Right}) ->
    {L, []} = parse_list(Left),
    {R, []} = parse_list(Right),
    ordered(L, R).

parse_list([$[ | T]) ->
    parse_list_impl([], T).

parse_list_impl(Acc, [$] | T]) ->
    {lists:reverse(Acc), T};
parse_list_impl(Acc, [$[ | _] = In) ->
    {List, Rest} = parse_list(In),
    parse_list_impl([List | Acc], Rest);
parse_list_impl(Acc, [$, | T]) ->
    parse_list_impl(Acc, T);
parse_list_impl(Acc, In) ->
    {N, Rest} = parse_number(In),
    parse_list_impl([N | Acc], Rest).

parse_number(L) ->
    parse_number_impl(0, L).

parse_number_impl(Acc, [H | T]) when H >= $0, H =< $9 ->
    parse_number_impl(Acc * 10 + (H - $0), T);
parse_number_impl(Acc, L) ->
    {Acc, L}.

ordered(L, L) when is_integer(L) ->
    equal;
ordered(L, R) when is_integer(L), is_integer(R), L < R ->
    true;
ordered(L, R) when is_integer(L), is_integer(R), L > R ->
    false;
ordered([], []) ->
    equal;
ordered([], [_ | _]) ->
    true;
ordered([_ | _], []) ->
    false;
ordered([LH | LT], [RH | RT]) ->
    case ordered(LH, RH) of
        true ->
            true;
        false ->
            false;
        equal ->
            ordered(LT, RT)
    end;
ordered(L, R) when is_list(L), is_integer(R) ->
    ordered(L, [R]);
ordered(L, R) when is_integer(L), is_list(R) ->
    ordered([L], R).

acc({I, Acc}, true) ->
    {I + 1, Acc + I};
acc({I, Acc}, false) ->
    {I + 1, Acc}.

fin({_, Acc}) ->
    Acc.
