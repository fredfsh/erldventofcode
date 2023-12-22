-module(aoc_2023_17_2).

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
            array:from_list(string:trim(X))
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

-record(state, {x, y, dx, dy, ttl}).
-define(a(X, Y), (array:get(X, array:get(Y, Matrix)) - $0)).
-define(STREAK, 10).
-define(MIN, 4).

fin(Matrix) ->
    State = #state{x = 0, y = 0, dx = 1, dy = 0, ttl = ?STREAK},
    Open = maps:from_list([{0, [State]}]),
    bfs(0, Open, sets:new(), Matrix).


bfs(I, Open, Closed, Matrix) ->
    %%io:format("bfs(~p) -- open: ~p, closed: ~p~n", [I, Open, Closed]),
    case maps:get(I, Open, []) of
        [] ->
            bfs(I + 1, Open, Closed, Matrix);
        L ->
            case bfs_impl(L, Open, Closed, I, Matrix) of
                N when is_integer(N) ->
                    N;
                {NewOpen, NewClosed} ->
                    bfs(I + 1, NewOpen, NewClosed, Matrix)
            end
    end.

bfs_impl([], Open, Closed, Loss, _Matrix) ->
    {maps:remove(Loss, Open), Closed};
bfs_impl([#state{x = X, y = Y} = State | T], Open, Closed, Loss, Matrix) ->
    Rows = array:size(Matrix),
    Cols = array:size(array:get(0, Matrix)),
    case {Rows - 1, Cols - 1} of
        {Y, X} ->
            Loss;
        _ ->
            case sets:is_element(State, Closed) of
                true ->
                    bfs_impl(T, Open, Closed, Loss, Matrix);
                false ->
                    NewClosed = sets:add_element(State, Closed),
                    NewOpen = bfs_state(Open, NewClosed, State, Loss, Matrix),
                    bfs_impl(T, NewOpen, NewClosed, Loss, Matrix)
            end
    end.

bfs_state(Open, Closed, State, Loss, Matrix) ->
    Nexts = nexts(State, Loss, Matrix),
    F = fun({Key, Val}, Acc) ->
                case sets:is_element(Key, Closed) of
                    true ->
                        Acc;
                    false ->
                        enopen(Acc, Key, Val)
                end
        end,
    lists:foldl(F, Open, Nexts).

nexts(#state{x = X, y = Y, dx = DX, dy = DY, ttl = TTL} = State, Loss, Matrix) ->
    {LDX, LDY} = turn_left(DX, DY),
    {RDX, RDY} = turn_right(DX, DY),
    L = [State#state{x = X + DX, y = Y + DY, ttl = TTL - 1},
         #state{x = X + LDX * ?MIN,
                y = Y + LDY * ?MIN,
                dx = LDX,
                dy = LDY,
                ttl = ?STREAK - ?MIN},
         #state{x = X + RDX * ?MIN,
                y = Y + RDY * ?MIN,
                dx = RDX,
                dy = RDY,
                ttl = ?STREAK - ?MIN}
        ],
    Rows = array:size(Matrix),
    Cols = array:size(array:get(0, Matrix)),
    F = fun(#state{x = SX, y = SY, ttl = STTL})
              when SX < 0; SX >= Cols; SY < 0; SY >= Rows; STTL < 0 ->
                false;
           (S) ->
                {true, next(Loss, Matrix, State, S)}
        end,
    lists:filtermap(F, L).

next(Loss, Matrix, #state{x=FX, y=FY}, #state{x=TX, y=TY, dx=DX, dy=DY} = To) ->
    {To, loss(Loss, FX, FY, {TX, TY, DX, DY, Matrix})}.

loss(Acc, NX, NY, {NX, NY, _, _, _}) ->
    Acc;
loss(Acc, X, Y, {_NX, _NY, NDX, NDY, Matrix} = Meta) ->
    loss(Acc + ?a(X + NDX, Y + NDY), X + NDX, Y + NDY, Meta).

turn_left( 1,  0) -> { 0, -1};
turn_left( 0, -1) -> {-1,  0};
turn_left(-1,  0) -> { 0,  1};
turn_left( 0,  1) -> { 1,  0}.

turn_right(DX0, DY0) ->
    {DX1, DY1} = turn_left(DX0, DY0),
    {DX2, DY2} = turn_left(DX1, DY1),
    {DX3, DY3} = turn_left(DX2, DY2),
    {DX3, DY3}.

enopen(Open, State, Loss) ->
    F = fun(L) -> [State | L] end,
    maps:update_with(Loss, F, [State], Open).
