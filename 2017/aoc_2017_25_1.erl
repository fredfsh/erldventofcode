-module(aoc_2017_25_1).

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
    case io:fread("", "In state ~c:") of
        eof ->
            eof;
        {ok, [State]} ->
            {State, input_condition(0), input_condition(1)}
    end.

input_condition(_) ->
    io:get_line(""),
    {ok, [Write]} = io:fread("", " - Write the value ~d."),
    Dir = case io:fread("", " - Move one slot to the ~a") of
              {ok, ['left.']} ->
                  -1;
              {ok, ['right.']} ->
                  1
          end,
    {ok, [State]} = io:fread("", " - Continue with state ~c."),
    {Write, Dir, State}.

ini() ->
    {ok, [State]} = io:fread("", "Begin in state ~c."),
    {ok, [N]} = io:fread("", "Perform a diagnostic checksum after ~d steps."),
    {State, N, maps:new()}.

do(X) ->
    X.

acc({Start, N, Map}, {State, Cond0, Cond1}) ->
    {Start, N, maps:put(State, {Cond0, Cond1}, Map)}.

fin({Start, N, Map}) ->
    run(0, 0, Start, sets:new(), N, Map).

run(N, _, _, Set, N, _) ->
    sets:size(Set);
run(I, X, State, Set, N, Map) ->
    Cond = case sets:is_element(X, Set) of
               false ->
                   1;
               true ->
                   2
           end,
    {Write, Dir, Next} = element(Cond, maps:get(State, Map)),
    NewSet = case {Cond, Write} of
                 {1, 1} ->
                     sets:add_element(X, Set);
                 {2, 0} ->
                     sets:del_element(X, Set);
                 _ ->
                     Set
             end,
    run(I + 1, X + Dir, Next, NewSet, N, Map).
