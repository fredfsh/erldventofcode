-module(aoc_2025_7_2).

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
        {ok, [X]} ->
            array:from_list(X)
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    [Header | Graph] = lists:reverse(X),
    Beam = length(hd(string:lexemes(array:to_list(Header), "S"))),
    splits(#{Beam => 1}, Graph).

splits(Timelines, [_]) ->
    lists:sum(maps:values(Timelines));
splits(Timelines, [_, H | T]) ->
    F = fun(Beam, Count, Acc) ->
                case array:get(Beam, H) of
                    $^ ->
                        inc(Beam - 1, Count, inc(Beam + 1, Count, Acc));
                    $. ->
                        inc(Beam, Count, Acc)
                end
        end,
    splits(maps:fold(F, #{}, Timelines), T).

inc(K, V, Map) ->
    maps:update_with(K, fun(X) -> X + V end, V, Map).
