-module(aoc_2022_9_1).

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
    case io:fread("", "~s ~d") of
        eof ->
            eof;
        {ok, [DirStr, Steps]} ->
            {list_to_atom(DirStr), Steps}
    end.

-record(state, {hx, hy, tx, ty, visited}).

ini() ->
    #state{hx = 0, hy = 0, tx = 0, ty = 0, visited = sets:from_list([{0, 0}])}.

do(X) ->
    X.

acc(Acc, {Dir, Steps}) ->
    F = fun(_, State) -> sim(State, Dir) end,
    lists:foldl(F, Acc, lists:seq(1, Steps)).

sim(#state{hx = HX, hy = HY, tx = TX, ty = TY, visited = Visited}, Dir) ->
    {DX, DY} = dxy(Dir),
    {NHX, NHY} = {HX + DX, HY + DY},
    NTXY = {NTX, NTY} = tail(NHX, NHY, TX, TY),
    NVisited = sets:add_element(NTXY, Visited),
    #state{hx = NHX, hy = NHY, tx = NTX, ty = NTY, visited = NVisited}.

dxy('U') -> { 0,  1};
dxy('D') -> { 0, -1};
dxy('L') -> {-1,  0};
dxy('R') -> { 1,  0}.

tail(HX, HY, TX, _TY) when TX - HX =:= 2; TX - HX =:= -2 ->
    {(TX + HX) div 2, HY};
tail(HX, HY, _TX, TY) when TY - HY =:= 2; TY - HY =:= -2 ->
    {HX, (TY + HY) div 2};
tail(_HX, _HY, TX, TY) ->
    {TX, TY}.

fin(#state{visited = Visited}) ->
    sets:size(Visited).
