-module(aoc_2022_19_1).

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

-define(FMT,
        "Blueprint ~d:
Each ore robot costs ~d ore.
Each clay robot costs ~d ore.
Each obsidian robot costs ~d ore and ~d clay.
Each geode robot costs ~d ore and ~d obsidian.").

input() ->
    case io:fread("", ?FMT) of
        eof ->
            eof;
        {ok, [ID, OO, CO, ObO, ObC, GO, GOb]} ->
            {ID, {OO, CO, {ObO, ObC}, {GO, GOb}}}
    end.

-define(TAB, memo).

ini() ->
    ets:new(?TAB, [named_table]),
    0.

-record(s, {o_r=1, c_r=0, ob_r=0, g_r=0, o=0, c=0, ob=0, g=0, min=0}).

do({ID, Blueprint}) ->
    ets:delete_all_objects(?TAB),
    S = #s{},
    ets:insert_new(?TAB, {key(S), 0}),
    Geode = dfs(S, 0, Blueprint),
%%    io:format("Blueprint #~p: ~p~n", [ID, Level]),
    ID * Geode.

key(#s{o_r=OR, c_r=CR, ob_r=ObR, g_r=GR, o=O, c=C, ob=Ob, g=G}) ->
    {OR, CR, ObR, GR, O, C, Ob, G}.

-define(MINUTES, 24).
-define(N, (?MINUTES - Min)).

%% optimization: stop at ?MINUTES - 1
dfs(#s{min = Min, g_r = GR, g = G}, AtLeast, _Blueprint)
  when Min =:= ?MINUTES - 1 ->
    max(AtLeast, G + GR);
%% optimization: build geode robot each turn if possible
dfs(#s{o_r=OR, ob_r=ObR, g_r=GR, g=G, min=Min}, AtLeast, {_, _, _, {GO, GOb}})
  when OR >= GO, ObR >= GOb ->
    max(AtLeast, G + GR * ?N + ?N * (?N - 1) div 2);
dfs(#s{g_r = GR, g = G, min = Min} = S, AtLeast, Blueprint) ->
    robot(S, max(AtLeast, G + GR * ?N), Blueprint).

%% choose the next robot to build, with the following pruning
%% p0: if the #robot exceeds the needs for building other robots, skip
%% p1: if the robot cannot be built by ?MINUTES - 1, skip
%% p2: if the updated maximum (build geode robot each turn) is not better than
%%     the known maximum, skip
%% p3: if the state has been reached with an earlier minutes, skip
robot(S, AtLeast, Blueprint) ->
    AccRG = geode_robot(S, AtLeast, Blueprint),
    AccROb = obsidian_robot(S, AccRG, Blueprint),
    AccRC = clay_robot(S, AccROb, Blueprint),
    AccRO = ore_robot(S, AccRC, Blueprint),
    AccRO.

geode_robot(#s{ob_r = 0}, AtLeast, _) ->
    AtLeast;
geode_robot(#s{o_r = OR, c_r = CR, ob_r = ObR, g_r = GR,
               o = O, c = C, ob = Ob, g = G, min = Min} = S,
            AtLeast,
            {_, _, _, {GO, GOb}} = Blueprint) ->
    %% #turns to wait until it can be built at (Turn + 1)
    Turn = max((max(0, GO-O)+(OR-1)) div OR, (max(0, GOb-Ob)+(ObR-1)) div ObR),
    case Min + Turn + 1 >= ?MINUTES of
        true ->  % p1
            AtLeast;
        false ->
            State = S#s{g_r = GR + 1,
                        o = O + OR * (Turn + 1) - GO,
                        c = C + CR * (Turn + 1),
                        ob = Ob + ObR * (Turn + 1) - GOb,
                        g = G + GR * (Turn + 1),
                        min = Min + Turn + 1},
            case maximum(State, GO, GOb) =< AtLeast of
                true ->  % #p2
                    AtLeast;
                false ->
                    Key = key(State),
                    case ets:lookup(?TAB, Key) of
                        [{_, Minutes}] when Minutes =< Min + Turn + 1 ->  % p3
                            AtLeast;
                        _ ->
                            ets:insert(?TAB, {Key, Min + Turn + 1}),
                            max(AtLeast, dfs(State, AtLeast, Blueprint))
                    end
            end
    end.

%% p0
obsidian_robot(#s{c_r = 0}, AtLeast, _) ->
    AtLeast;
obsidian_robot(#s{ob_r = ObR}, AtLeast, {_, _, _, {_, GOb}}) when ObR >= GOb ->
    AtLeast;
obsidian_robot(#s{o_r = OR, c_r = CR, ob_r = ObR, g_r = GR,
                  o = O, c = C, ob = Ob, g = G, min = Min} = S,
               AtLeast,
               {_, _, {ObO, ObC}, {GO, GOb}} = Blueprint) ->
    %% #turns to wait until it can be built at (Turn + 1)
    Turn = max((max(0, ObO-O)+(OR-1)) div OR, (max(0, ObC-C)+(CR-1)) div CR),
    case Min + Turn + 1 >= ?MINUTES of
        true ->  % p1
            AtLeast;
        false ->
            State = S#s{ob_r = ObR + 1,
                        o = O + OR * (Turn + 1) - ObO,
                        c = C + CR * (Turn + 1) - ObC,
                        ob = Ob + ObR * (Turn + 1),
                        g = G + GR * (Turn + 1),
                        min = Min + Turn + 1},
            case maximum(State, GO, GOb) =< AtLeast of
                true ->  % #p2
                    AtLeast;
                false ->
                    Key = key(State),
                    case ets:lookup(?TAB, Key) of
                        [{_, Minutes}] when Minutes =< Min + Turn + 1 ->  % p3
                            AtLeast;
                        _ ->
                            ets:insert(?TAB, {Key, Min + Turn + 1}),
                            max(AtLeast, dfs(State, AtLeast, Blueprint))
                    end
            end
    end.

%% p0
clay_robot(#s{c_r = CR}, AtLeast, {_, _, {_, ObC}, {_, _}}) when CR >= ObC ->
    AtLeast;
clay_robot(#s{o_r = OR, c_r = CR, ob_r = ObR, g_r = GR,
              o = O, c = C, ob = Ob, g = G, min = Min} = S,
           AtLeast,
           {_, CO, _, {GO, GOb}} = Blueprint) ->
    %% #turns to wait until it can be built at (Turn + 1)
    Turn = (max(0, CO - O) + (OR - 1)) div OR,
    case Min + Turn + 1 >= ?MINUTES of
        true ->  % p1
            AtLeast;
        false ->
            State = S#s{c_r = CR + 1,
                        o = O + OR * (Turn + 1) - CO,
                        c = C + CR * (Turn + 1),
                        ob = Ob + ObR * (Turn + 1),
                        g = G + GR * (Turn + 1),
                        min = Min + Turn + 1},
            case maximum(State, GO, GOb) =< AtLeast of
                true ->  % #p2
                    AtLeast;
                false ->
                    Key = key(State),
                    case ets:lookup(?TAB, Key) of
                        [{_, Minutes}] when Minutes =< Min + Turn + 1 ->  % p3
                            AtLeast;
                        _ ->
                            ets:insert(?TAB, {Key, Min + Turn + 1}),
                            max(AtLeast, dfs(State, AtLeast, Blueprint))
                    end
            end
    end.

%% p0
ore_robot(#s{o_r = OR}, AtLeast, {OO, CO, {ObO, _}, {GO, _}})
  when OR >= OO, OR >= CO, OR >= ObO, OR >= GO ->
    AtLeast;
ore_robot(#s{o_r = OR, c_r = CR, ob_r = ObR, g_r = GR,
             o = O, c = C, ob = Ob, g = G, min = Min} = S,
          AtLeast,
          {OO, _, _, {GO, GOb}} = Blueprint) ->
    %% #turns to wait until it can be built at (Turn + 1)
    Turn = (max(0, OO - O) + (OR - 1)) div OR,
    case Min + Turn + 1 >= ?MINUTES of
        true ->  % p1
            AtLeast;
        false ->
            State = S#s{o_r = OR + 1,
                        o = O + OR * (Turn + 1) - OO,
                        c = C + CR * (Turn + 1),
                        ob = Ob + ObR * (Turn + 1),
                        g = G + GR * (Turn + 1),
                        min = Min + Turn + 1},
            case maximum(State, GO, GOb) =< AtLeast of
                true ->  % #p2
                    AtLeast;
                false ->
                    Key = key(State),
                    case ets:lookup(?TAB, Key) of
                        [{_, Minutes}] when Minutes =< Min + Turn + 1 ->  % p3
                            AtLeast;
                        _ ->
                            ets:insert(?TAB, {Key, Min + Turn + 1}),
                            max(AtLeast, dfs(State, AtLeast, Blueprint))
                    end
            end
    end.

%% calculate the theoretical maximum -- building geode robot each turn
maximum(#s{g_r = GR, o = O, ob = Ob, g = G, min = Min}, GO, GOb)
  when O >= GO, Ob >= GOb ->
    G + GR * ?N + ?N * (?N - 1) div 2;
maximum(#s{g_r = GR, g = G, min = Min}, _, _) ->
    G + GR * ?N + ?N * (?N - 1) div 2 - (?N - 1).

%% bfs(Acc, Q, Seen, AtLeast, {_, _, _, {GO, GOb}} = Blueprint) ->
%% %%    io:format("~p~n", [AtLeast]),
%%     case queue:out(Q) of
%%         {empty, _} ->
%%             Acc;
%%         %% optimization: stop at ?MINUTES - 1
%%         {{value, #s{min = Min, g_r = GR, g = G}}, Q2} when Min =:= ?MINUTES-1 ->
%%             bfs(max(Acc, G + GR), Q2, Seen, max(AtLeast, G + GR), Blueprint);
%%         %% optimization: build geode robot each turn if possible
%%         {{value, #s{o_r = OR, ob_r = ObR, g_r = GR, g = G, min = Min} = S},
%%          Q2} when OR >= GO, ObR >= GOb ->
%%             N = ?MINUTES - Min,
%%             Geode = G + GR * N + N * (N - 1) div 2,
%%             bfs(max(Acc, Geode), Q2, Seen, max(AtLeast, Geode), Blueprint);
%%         %% optimization: if geode robot can be built, then building it is
%%         %%               better than any other strategy
%%         {{value, #s{o_r=OR,c_r=CR,ob_r=ObR,g_r=GR,o=O,c=C,ob=Ob,g=G,min=Min}=S},
%%          Q2} when O >= GO, Ob >= GOb ->
%%             N = ?MINUTES - Min,
%%             AtMost = G + GR * N + N * (N - 1) div 2,
%%             case AtMost >= AtLeast of
%%                 true ->
%%                     SG = S#s{g_r = GR + 1, o = O - GO + OR, ob = Ob - GOb + ObR,
%%                              c = C + CR, g = G + GR, min = Min + 1},
%%                     {NQ, NSeen} = qseen(SG, Q2, Seen),
%%                     bfs(Acc, NQ, NSeen, max(AtLeast, G+GR*N+N-1), Blueprint);
%%                 false ->
%%                     bfs(Acc, Q2, Seen, max(AtLeast, G + GR * N), Blueprint)
%%             end;
%%         {{value, #s{g_r = GR, g = G, min = Min} = S}, Q2} ->
%%             N = ?MINUTES - Min,
%%             AtMost = G + GR * N + N * (N - 1) div 2 - N + 1,
%%             case AtMost >= AtLeast of
%%                 true ->
%%                     {NQ, NSeen} = bfs_impl(Q2, Seen, S, Blueprint),
%%                     bfs(Acc, NQ, NSeen, max(AtLeast, G + GR * N), Blueprint);
%%                 false ->
%%                     bfs(Acc, Q2, Seen, max(AtLeast, G + GR * N), Blueprint)
%%             end
%%     end.


%% bfs_impl(Q,
%%          Seen,
%%          #s{o_r=OR, c_r=CR, ob_r=ObR, g_r=GR, o=O, c=C, ob=Ob, g=G, min=Min} = S,
%%          {OO, CO, {ObO, ObC}, {GO, GOb}}) ->
%%     S0 = S#s{o = O + OR, ob = Ob + ObR, c = C + CR, g = G + GR, min = Min + 1},
%%     {Q0, Seen0} = qseen(S0, Q, Seen),
%%     {Q1, Seen1} = case O >= OO andalso (OR < OO orelse
%%                                         OR < CO orelse
%%                                         OR < ObO orelse
%%                                         OR < GO) of
%%                       true ->
%%                           S1 = S0#s{o_r = OR + 1, o = O - OO + OR},
%%                           qseen(S1, Q0, Seen0);
%%                       false ->
%%                           {Q0, Seen0}
%%                   end,
%%     {Q2, Seen2} = case O >= CO andalso CR < ObC of
%%                       true ->
%%                           S2 = S0#s{c_r = CR + 1, o = O - CO + OR},
%%                           qseen(S2, Q1, Seen1);
%%                       false ->
%%                           {Q1, Seen1}
%%                   end,
%%     {Q3, Seen3} = case O >= ObO andalso C >= ObC andalso Ob < GOb of
%%                       true ->
%%                           S3 = S0#s{ob_r=ObR+1, o=O-ObO+OR, c=C-ObC+CR},
%%                           qseen(S3, Q2, Seen2);
%%                       false ->
%%                           {Q2, Seen2}
%%                   end,
%%     {Q3, Seen3}.

%% qseen(State, Q, Seen) ->
%% %%    io:format("~p~n", [State]),
%%     K = key(State),
%%     case sets:is_element(K, Seen) of
%%         true ->
%%             {Q, Seen};
%%         false ->
%%             {queue:in(State, Q), sets:add_element(K, Seen)}
%%     end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
