-module(aoc_2022_23_2).

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
            X
    end.

ini() ->
    {0, sets:new()}.

do(X) ->
    X.

acc({Y, Acc}, Row) ->
    F = fun($#, {X, FAcc}) ->
                {X + 1, sets:add_element({X, Y}, FAcc)};
           (_, {X, FAcc}) ->
                {X + 1, FAcc}
        end,
    {_, Elves} = lists:foldl(F, {0, Acc}, Row),
    {Y + 1, Elves}.

fin({_, X}) ->
    sim(0, X).

sim(Rounds, Elves) ->
    Proposals = propose(Elves, Rounds),
    case still(Proposals) of
        true ->
            Rounds + 1;
        false ->
            NewElves = move(Elves, Rounds, Proposals),
            sim(Rounds + 1, NewElves)
    end.

propose(Elves, Rounds) ->
    F = fun({X, Y}, Acc) ->
                case next(X, Y, Elves, Rounds) of
                    undefined ->
                        Acc;
                    NXY ->
                        maps:update_with(NXY, fun inc/1, 1, Acc)
                end
        end,
    sets:fold(F, maps:new(), Elves).

-define(CHECKS, [{0, -1}, {0, 1}, {-1, 0}, {1, 0}]).

next(X, Y, Elves, Rounds) ->
    Neighbors = neighbors(X, Y, Elves),
    case sets:size(Neighbors) of
        0 ->
            undefined;
        _ ->
            {L1, L2} = lists:split(Rounds rem length(?CHECKS), ?CHECKS),
            Checks = lists:append(L2, L1),
            F = fun({0, -1}) ->
                        sets:is_element(n, Neighbors) orelse
                             sets:is_element(ne, Neighbors) orelse
                             sets:is_element(nw, Neighbors);
                   ({0, 1}) ->
                        sets:is_element(s, Neighbors) orelse
                            sets:is_element(se, Neighbors) orelse
                            sets:is_element(sw, Neighbors);
                   ({-1, 0}) ->
                        sets:is_element(w, Neighbors) orelse
                             sets:is_element(nw, Neighbors) orelse
                             sets:is_element(sw, Neighbors);
                   ({1, 0}) ->
                        sets:is_element(e, Neighbors) orelse
                             sets:is_element(ne, Neighbors) orelse
                             sets:is_element(se, Neighbors)
                end,
            case lists:dropwhile(F, Checks) of
                [] ->
                    undefined;
                [{DX, DY} | _] ->
                    {X + DX, Y + DY}
            end
    end.

-define(D, [{ n,  0, -1}, { s , 0,  1}, { w, -1,  0}, { e,  1,  0},
            {ne,  1, -1}, {nw, -1, -1}, {se,  1,  1}, {sw, -1,  1}]).

neighbors(X, Y, Elves) ->
    sets:from_list([D || {D,DX,DY} <- ?D, sets:is_element({X+DX,Y+DY}, Elves)]).

inc(X) -> X + 1.

still(Proposals) ->
    F = fun(_, V) -> V =:= 1 end,
    maps:filter(F, Proposals) =:= #{}.

move(Elves, Rounds, Proposals) ->
    F = fun({X, Y}, Acc) ->
                {NX, NY} = case next(X, Y, Elves, Rounds) of
                               undefined ->
                                   {X, Y};
                               NXY ->
                                   case maps:get(NXY, Proposals) of
                                       1 ->
                                           NXY;
                                       _ ->
                                           {X, Y}
                                   end
                           end,
                sets:add_element({NX, NY}, Acc)
        end,
    sets:fold(F, sets:new(), Elves).
