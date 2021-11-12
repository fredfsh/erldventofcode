-module(aoc_2018_4_1).

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
    case io:fread("", "[~s ~d:~d]") of
        eof ->
            eof;
        {ok, [Date, HH, MM]} ->
            What = case io:fread("", "~a") of
                       {ok, ['Guard']} ->
                           {ok, [Guard]} = io:fread("", " #~d"),
                           Guard;
                       {ok, [falls]} ->
                           sleep;
                       {ok, [wakes]} ->
                           wake
                   end,
            io:get_line(""),
            {{Date, HH, MM}, What}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    Logs = lists:sort(X),
    Sleeps = verbose(Logs),
    Who = sleepiest(Sleeps),
    Minute = minute(Sleeps, Who),
    Who * Minute.

verbose(Logs) ->
    verbose_impl(maps:new(), undefined, undefined, Logs).

verbose_impl(Map, _, _, []) ->
    Map;
verbose_impl(Map, _, _, [{_, Who} | T]) when is_integer(Who) ->
    verbose_impl(Map, Who, undefined, T);
verbose_impl(Map, Who, _, [{{_, _, MM}, sleep} | T]) ->
    verbose_impl(Map, Who, MM, T);
verbose_impl(Map, Who, Start, [{{_, _, MM}, wake} | T]) ->
    Minutes = maps:get(Who, Map, maps:new()),
    F = fun(Minute, Acc) ->
                maps:update_with(Minute, fun(V) -> V + 1 end, 1, Acc)
        end,
    NewMinutes = lists:foldl(F, Minutes, lists:seq(Start, MM - 1)),
    verbose_impl(maps:put(Who, NewMinutes, Map), Who, undefined, T).

sleepiest(Sleeps) ->
    L = maps:to_list(Sleeps),
    Sums = [{Who, sum(Minutes)} || {Who, Minutes} <- L],
    {Who, _} = lists:last(lists:keysort(2, Sums)),
    Who.

sum(Minutes) ->
    lists:sum([Count || {_, Count} <- maps:to_list(Minutes)]).

minute(Sleeps, Who) ->
    Minutes = maps:get(Who, Sleeps),
    L = maps:to_list(Minutes),
    {Minute, _} = lists:last(lists:keysort(2, L)),
    Minute.
