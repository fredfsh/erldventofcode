-module(aoc_2022_9_2).

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

-define(KNOTS, 10).

-record(state, {hx, hy, tl, visited}).

ini() ->
    #state{hx = 0,
           hy = 0,
           tl = array:new([?KNOTS - 1, {default, {0, 0}}]),
           visited = sets:from_list([{0, 0}])}.

do(X) ->
    X.

acc(Acc, {Dir, Steps}) ->
    F = fun(_, State) -> sim(State, Dir) end,
    lists:foldl(F, Acc, lists:seq(1, Steps)).

sim(#state{hx = HX, hy = HY, tl = Tails, visited = Visited}, Dir) ->
    {DX, DY} = dxy(Dir),
    {NHX, NHY} = {HX + DX, HY + DY},
    NTails = tail(NHX, NHY, Tails),
    NVisited = sets:add_element(array:get(?KNOTS - 2, NTails), Visited),
    #state{hx = NHX, hy = NHY, tl = NTails, visited = NVisited}.

dxy('U') -> { 0,  1};
dxy('D') -> { 0, -1};
dxy('L') -> {-1,  0};
dxy('R') -> { 1,  0}.

tail(HX, HY, Tails) ->
    F = fun(_, {TX, TY}, {HXAcc, HYAcc, Acc}) ->
                NTXY = {NTX, NTY} = tail(HXAcc, HYAcc, TX, TY),
                {NTX, NTY, array:set(array:size(Acc), NTXY, Acc)}
        end,
    {_, _, Res} = array:foldl(F, {HX, HY, array:new()}, Tails),
    Res.

tail(HX, HY, TX, TY) when (TX - HX =:= 2 orelse TX - HX =:= -2) andalso
                          (TY - HY =:= 2 orelse TY - HY =:= -2) ->
    {(TX + HX) div 2, (TY + HY) div 2};
tail(HX, HY, TX, _TY) when TX - HX =:= 2; TX - HX =:= -2 ->
    {(TX + HX) div 2, HY};
tail(HX, HY, _TX, TY) when TY - HY =:= 2; TY - HY =:= -2 ->
    {HX, (TY + HY) div 2};
tail(_HX, _HY, TX, TY) ->
    {TX, TY}.

fin(#state{visited = Visited}) ->
    sets:size(Visited).
