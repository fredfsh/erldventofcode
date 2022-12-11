-module(aoc_2022_10_2).

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
    case io:fread("", "~a") of
        eof ->
            eof;
        {ok, [noop]} ->
            noop;
        {ok, [addx]} ->
            {ok, [V]} = io:fread("", " ~d"),
            {addx, V}
    end.

-record(cpu, {x, cycle, crt}).

ini() ->
    #cpu{x = 1, cycle = 0, crt = array:new({default, array:new()})}.

do(X) ->
    X.

acc(#cpu{x = X, cycle = Cycle, crt = CRT} = CPU, noop) ->
    %% during (Cycle + 1)
    NCRT = crt_set(Cycle + 1, X, CRT),
    %% after (Cycle + 1)
    CPU#cpu{cycle = Cycle + 1, crt = NCRT};
acc(#cpu{x = X, cycle = Cycle, crt = CRT}, {addx, V}) ->
    %% during (Cycle + 1)
    NCRT = crt_set(Cycle + 1, X, CRT),
    %% during (Cycle + 2)
    NNCRT = crt_set(Cycle + 2, X, NCRT),
    %% after (Cycle + 2)
    #cpu{x = X + V, cycle = Cycle + 2, crt = NNCRT}.

-define(COL, 40).

crt_set(Cycle, Pos, CRT) ->
    Row = (Cycle - 1) div ?COL,
    Col = (Cycle - 1) rem ?COL,
    Val = case (Pos - 1 =< Col andalso Pos + 1 >= Col) of
              true ->
                  $#;
              false ->
                  $.
          end,
    Arr = array:get(Row, CRT),
    NArr = array:set(Col, Val, Arr),
    array:set(Row, NArr, CRT).

fin(#cpu{crt = CRT}) ->
    print(CRT).

print(CRT) ->
    F = fun(_, Row, Acc) ->
                io:format("~s\n", [array:to_list(Row)]),
                Acc
        end,
    array:foldl(F, undefined, CRT).
