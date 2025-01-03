-module(aoc_2024_18_1).

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
    case io:fread("", "~d,~d") of
        eof ->
            eof;
        {ok, [X, Y]} ->
            {X, Y}
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

-define(K, 1024).

fin(L) ->
    Walls = sets:from_list(lists:sublist(lists:reverse(L), ?K)),
    bfs(queue:from_list([{0, 0, 0}]), sets:from_list([{0, 0}]), Walls).

-define(N, 70).
-define(D, [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]).

bfs(Q, Seen, Walls) ->
    {{value, {X, Y, Steps}}, Q2} = queue:out(Q),
%%    io:format("~p~n~p~n", [{X, Y, Steps}, queue:to_list(Q2)]),
    case X =:= ?N andalso Y =:= ?N of
        true ->
            Steps;
        false ->
            F = fun({DX, DY}, {QAcc, SeenAcc} = Acc) ->
                        {NX, NY} = NXY = {X + DX, Y + DY},
%%                        io:format("NXY: ~p~n", [NXY]),
                        case NX < 0 orelse NX > ?N orelse NY < 0 orelse NY > ?N
                            orelse sets:is_element(NXY, SeenAcc)
                            orelse sets:is_element(NXY, Walls) of
                            true ->
                                Acc;
                            false ->
%%                                io:format("adding ~p~n", [NXY]),
                                {queue:in({NX, NY, Steps + 1}, QAcc),
                                 sets:add_element(NXY, SeenAcc)}
                        end
                end,
            {NewQ, NewSeen} = lists:foldl(F, {Q2, Seen}, ?D),
            bfs(NewQ, NewSeen, Walls)
    end.
