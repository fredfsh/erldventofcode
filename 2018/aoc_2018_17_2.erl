-module(aoc_2018_17_2).

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
    case io:fread("", "~c=~d, ~c=~d..~d") of
        eof ->
            eof;
        {ok, [Axis, V, _, A, B]} ->
            {list_to_atom(Axis), V, A, B}
    end.

-define(TAB, tab).

ini() ->
    ets:new(?TAB, [named_table]),
    {undefined, -1}.

do(X) ->
    X.

acc({Min, Max}, {x, X, A, B}) ->
    F = fun(Y, {MinAcc, MaxAcc}) ->
                ets:insert(?TAB, {{X, Y}, clay}),
                {min(MinAcc, Y), max(MaxAcc, Y)}
        end,
    lists:foldl(F, {Min, Max}, lists:seq(A, B));
acc({Min, Max}, {y, Y, A, B}) ->
    ets:insert(?TAB, [{{X, Y}, clay} || X <- lists:seq(A, B)]),
    {min(Min, Y), max(Max, Y)}.

-define(SPRING, 500).

fin({Min, Max}) ->
    dp(?SPRING, 1, Max),
    count(Min).

dp(X, Y, Max) ->
    case ets:lookup(?TAB, {X, Y}) of
        [] ->
            water(X, Y, Max);
        [{_, What}] ->
            What
    end.

water(X, Max, Max) ->
    ets:insert_new(?TAB, {{X, Max}, stream}),
    stream;
water(X, Y, Max) ->
    case dp(X, Y + 1, Max) of
        stream ->
            ets:insert_new(?TAB, {{X, Y}, stream}),
            stream;
        _ ->
            spread(X, X, Y, Max)
    end.

spread(Left, Right, Y, Max) ->
    {L, R, Downstreams} = expand(Left, Right, Y),
    F = fun({XX, YY}, Acc) ->
                Acc or (dp(XX, YY, Max) =:= stream)
        end,
    case lists:foldl(F, false, Downstreams) of
        true ->
            fill(L, R, Y, stream);
        false ->
            case {Left, Right} of
                {L, R} ->
                    fill(L, R, Y, water);
                _ ->
                    spread(L, R, Y, Max)
            end
    end.

fill(Left, Right, Y, Water) ->
    Inserts = [{{X, Y}, Water} || X <- lists:seq(Left, Right)],
    ets:insert_new(?TAB, Inserts),
    Water.

expand(Left, Right, Y) ->
    {L, Acc} = expand_impl([], -1, Left, Y),
    {R, Acc2} = expand_impl(Acc, 1, Right, Y),
    {L, R, Acc2}.

expand_impl(Downstreams, DX, X, Y) ->
    case ets:lookup(?TAB, {X + DX, Y}) of
        [{_, clay}] ->
            {X, Downstreams};
        _ ->
            case ets:lookup(?TAB, {X + DX, Y + 1}) of
                [] ->
                    {X + DX, [{X + DX, Y + 1} | Downstreams]};
                _ ->
                    expand_impl(Downstreams, DX, X + DX, Y)
            end
    end.

count(Min) ->
    Spec = [{{{'_','$1'},'$2'}, [{'>=','$1',Min}, {'=:=','$2',water}], [true]}],
    ets:select_count(?TAB, Spec).
