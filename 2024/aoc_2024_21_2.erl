-module(aoc_2024_21_2).

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
    ini_keypads(),
    0.

-define(TAB, memo).
-define(NUMERIC, numeric).
-define(DIRECTIONAL, directional).

ini_keypads() ->
    ets:new(?TAB, [named_table]),
    ini_keypad(?DIRECTIONAL),
    ini_keypad(?NUMERIC).

%% e.g. "029A"
%%
%% A|<vA<AA>>^AvAA<^A>A|<v<A>>^AvA^A|<vA>^A<v<A>^A>AAvA^A|<v<A>A>^AAAvA<^A>A
%% A|^vv<<<v>AA>>>v^^AA|^v<<v>AA>>AA|^vv>AA^v<<v^^AAA>>AA|^v<<vv>AAAA>>v^^AA
%%  |  v <<   A >>  ^ A|   <   A > A|  v  A   <  ^ AA > A|   < v  AAA >  ^ A
%% A|AA>>v<<<<<<v>>>AAA|AAA^^^^^^AAA|AA>>>>>>>vvv^^^^^AAA|AAA^^vvvvvvv>>>AAA
%%  |         <       A|       ^   A|     >        ^^   A|        vvv      A
%% A|AAAAAAAAA000000000|000000022222|22222333333333699999|9999999963AAAAAAAA
%%  |                 0|           2|                   9|                 A

-define(INVALID, invalid).

%% +---+---+---+      +---+---+
%% | 7 | 8 | 9 |      | ^ | A |
%% +---+---+---+  +---+---+---+
%% | 4 | 5 | 6 |  | < | v | > |
%% +---+---+---+  +---+---+---+
%% | 1 | 2 | 3 |
%% +---+---+---+
%%     | 0 | A |
%%     +---+---+
-define(KEYPADS, #{?NUMERIC => #{$7 => {0, 0},
                                 $8 => {1, 0},
                                 $9 => {2, 0},
                                 $4 => {0, 1},
                                 $5 => {1, 1},
                                 $6 => {2, 1},
                                 $1 => {0, 2},
                                 $2 => {1, 2},
                                 $3 => {2, 2},
                                 ?INVALID => {0, 3},
                                 $0 => {1, 3},
                                 $A => {2, 3}},
                   ?DIRECTIONAL => #{?INVALID => {0, 0},
                                     $^ => {1, 0},
                                     $A => {2, 0},
                                     $< => {0, 1},
                                     $v => {1, 1},
                                     $> => {2, 1}}}).

ini_keypad(Type) ->
    Keypad = maps:get(Type, ?KEYPADS),
    Keys = [K || K <- maps:keys(Keypad), K =/= ?INVALID],
    [ets:insert_new(?TAB, {{K, K}, "A"}) || K <- Keys, K =/= ?INVALID],

    [first_pass(S, T, Type) || S <- Keys, T <- Keys, S =/= T],
    second_pass().

%% Preprocess for two passes to come up with a control sequence for each route.

%% In the first pass, change direction as little as possible.  e.g. from 0
%% to 9 we record both ">^^^A" or "^^^>A" but not "^>^^A".
first_pass(S, T, Type) ->
    Keypad = maps:get(Type, ?KEYPADS),
    {SX, SY} = maps:get(S, Keypad),
    {TX, TY} = maps:get(T, Keypad),
    YSeq = case TY - SY of
               0 ->
                   "";
               PY when PY > 0 ->
                   lists:duplicate(PY, $v);
               NY when NY < 0 ->
                   lists:duplicate(-NY, $^)
           end,
    XSeq = case TX - SX of
               0 ->
                   "";
               PX when PX > 0 ->
                   lists:duplicate(PX, $>);
               NX when NX < 0 ->
                   lists:duplicate(-NX, $<)
           end,
    F = fun(Seq) ->
                G = fun(_, false) ->
                            false;
                       (Move, {X, Y}) ->
                            {DX, DY} = d(Move),
                            {IX, IY} = maps:get(?INVALID, Keypad),
                            case {X + DX, Y + DY} of
                                {IX, IY} ->
                                    false;
                                NXY ->
                                    NXY
                            end
                    end,
                case lists:foldl(G, {SX, SY}, Seq) of
                    false ->
                        false;
                    {_, _} ->
                        true
                end
        end,
    Res1 = case {XSeq, YSeq} of
               {"", _} ->
                   [YSeq];
               {_, ""} ->
                   [XSeq];
               _ ->
                   lists:filter(F, [XSeq ++ YSeq, YSeq ++ XSeq])
           end,
    Res2 = [Seq ++ "A" || Seq <- Res1],
    Res3 = case Res2 of
               [Res] ->
                   Res;
               [Left, Right] ->
                   {Left, Right}
           end,
    ets:insert_new(?TAB, {{S, T}, Res3}).

d($^) -> { 0, -1};
d($v) -> { 0,  1};
d($<) -> {-1,  0};
d($>) -> { 1,  0}.


%% In the second pass, breaking ties by picking the route that would generate
%% fewer number of presses (based on first pass results) at a higher level.
%% Adding one more level of robot at a each time.  This resolves all ties at
%% or below level 4.
%% e.g. from 0 to 9 "^^^>A" is picked over ">^^^A" (at level 4 with 119 vs 125).
second_pass() ->
%%    io:format("~p~n", [ets:tab2list(?TAB)]),
    F = fun({Key, {Left, Right}}, _) ->
                ets:insert(?TAB, {Key, break_tie(Left, Right)});
           (_, _) ->
                ok
        end,
    ets:foldl(F, undefined, ?TAB).

break_tie(Left, Right) ->
%%    io:format("breaking tie: ~p vs ~p~n", [Left, Right]),
    %% using same table to cache comparison results
    case ets:lookup_element(?TAB, {Left, Right}, 2, undefined) of
        undefined ->
            Res = break_tie_impl([Left], [Right], Left, Right),
            ets:insert_new(?TAB, [{{Left, Right}, Res}, {{Right, Left}, Res}]),
            Res;
        Res ->
            Res
    end.

break_tie_impl(Lefts, Rights, Left, Right) ->
%%    io:format("breaking tie impl: ~p vs ~p~n", [Lefts, Rights]),
    case {lists:min([length(LS) || LS <- Lefts]),
          lists:min([length(RS) || RS <- Rights])} of
        {Less, More} when Less < More ->
            Left;
        {More, Less} when More > Less ->
            Right;
        _ ->
            break_tie_impl(seqs(Lefts), seqs(Rights), Left, Right)
    end.

%% given a list of sequences, returns a list of *control* sequences
%% at a higher level, with the minimal length, among all
%% such sequences with arbitrary length
%%
%% e.g. suppose C => A, D => A, E => B where len(C) = len(E) < len(D),
%5 then [A, B] => [C, E]
seqs(Seqs) ->
    Candidates = lists:append([seq(S) || S <- Seqs]),
%%    io:format("seqs:~ninput: ~p~noutput: ~p~n", [Seqs, Candidates]),
    Lens = [{length(C), C} || C <- Candidates],
    {Len, _} = hd(lists:sort(Lens)),
    [C || {L, C} <- Lens, L =:= Len].

%% given a sequence, returns all *control* sequences at a higher level
%% e.g. "029A" => ["<A^A>^^AvvvA", "<A^A^^>AvvvA"]
seq(Seq) ->
%%    io:format("seq(~p)~n", [Seq]),
    F = fun(Next, {Current, Acc}) ->
%%                io:format("~p => ~p, acc: ~p~n", [Current, Next, Acc]),
                NewAcc = case ets:lookup_element(?TAB, {Current, Next}, 2) of
                             {Left, Right} ->
                                 lists:append([P ++ Left || P <- Acc],
                                              [P ++ Right || P <- Acc]);
                             Suffix ->
                                 [P ++ Suffix || P <- Acc]
                         end,
                {Next, NewAcc}
        end,
    {_, Res} = lists:foldl(F, {$A, [""]}, Seq),
%%    io:format("seq:~ninput: ~p~noutput: ~p~n", [Seq, Res]),
    Res.

-define(DIRECTIONALS, 25).

do(X) ->
    {N, _} = string:to_integer(X),
    seq_len(X) * N.

seq_len(Seq) ->
    Init = case ?DIRECTIONALS rem 2 of
               0 ->
                   fast_seq(Seq);
               1 ->
                   fast_seq_x(Seq, 2)
           end,
    Depth = ?DIRECTIONALS div 2,
    Map = mid_map(Depth),
    Fun = fun(S, T) -> element(1, maps:get({S, T}, Map)) end,
    MidSeq = fast_seq(Init, Fun),
    %%    io:format("mid seq: ~p~n", [MidSeq]),
    F = fun(Next, {Current, Acc}) ->
                {Next, Acc + element(2, maps:get({Current, Next}, Map))}
        end,
    {_, Res} = lists:foldl(F, {$A, 0}, MidSeq),
%%    io:format("res = ~p~n", [Res]),
    Res.

mid_map(Depth) ->
    Directional = maps:get(?DIRECTIONAL, ?KEYPADS),
    Keys = [K || K <- maps:keys(Directional), K =/= ?INVALID],
    L = [begin
             Init = ets:lookup_element(?TAB, {S, T}, 2),
             Seq = fast_seq_x(Init, Depth - 1),
             {{S, T}, {Seq, length(Seq)}}
         end || S <- Keys, T <- Keys],
    maps:from_list(L).

fast_seq(Seq) ->
    fast_seq(Seq, fun(S, T) -> ets:lookup_element(?TAB, {S, T}, 2) end).
fast_seq(Seq, Fun) ->
    F = fun(Next, {Current, Acc}) ->
                Tr = Fun(Current, Next),
                {Next, [lists:reverse(Tr) | Acc]}
        end,
    {_, Rev} = lists:foldl(F, {$A, []}, Seq),
    lists:reverse(lists:append(Rev)).

fast_seq_x(Seq, 0) ->
    Seq;
fast_seq_x(Seq, X) ->
    fast_seq_x(fast_seq(Seq), X - 1).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.

%% print_ets() ->
%%     io:format("printing ets:~n"),
%%     F = fun({{S, T}, _} = Item, _) when is_list(S), is_list(T) ->
%%                 io:format("- ~p~n", [Item]);
%%            ({{S, T}, Seq}, _) ->
%%                 io:format("- ~s => ~p~n", [[S, T], Seq])
%%         end,
%%     ets:foldl(F, undefined, ?TAB).
