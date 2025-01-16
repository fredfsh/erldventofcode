-module(aoc_2024_21_1).

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
    0.

-define(INVALID, invalid).

%% +---+---+---+
%% | 7 | 8 | 9 |
%% +---+---+---+
%% | 4 | 5 | 6 |
%% +---+---+---+
%% | 1 | 2 | 3 |
%% +---+---+---+
%%     | 0 | A |
%%     +---+---+
-define(KEYPAD, #{$7 => {0, 0},
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
                  $A => {2, 3}}).

%%     +---+---+
%%     | ^ | A |
%% +---+---+---+
%% | < | v | > |
%% +---+---+---+
-define(CONTROLLER, #{?INVALID => {0, 0},
                      $^ => {1, 0},
                      $A => {2, 0},
                      $< => {0, 1},
                      $v => {1, 1},
                      $> => {2, 1}}).

do(X) ->
    Seqs1 = seqs([X], ?KEYPAD),
    Seqs2 = seqs(Seqs1, ?CONTROLLER),
    Seqs3 = seqs(Seqs2, ?CONTROLLER),

    {N, _} = string:to_integer(X),
    length(hd(Seqs3)) * N.

seqs(Seqs, Keymap) ->
    Candidates = lists:append([seq(S, Keymap) || S <- Seqs]),
%%    io:format("seqs:~ninput: ~p~noutput: ~p~n", [Seqs, Candidates]),
    Lens = [{length(C), C} || C <- Candidates],
    {Len, _} = hd(lists:sort(Lens)),
    [C || {L, C} <- Lens, L =:= Len].

seq(Seq, Keymap) ->
    F = fun(Next, {Current, Acc}) ->
                Suffixes = [P ++ "A" || P <- greedy(Current, Next, Keymap)],
%%                io:format("~p~n", [Suffixes]),
                {Next, [lists:append(P, S) || P <- Acc, S <- Suffixes]}
        end,
    {_, Res} = lists:foldl(F, {$A, [""]}, Seq),
%%    io:format("seq:~ninput: ~p~noutput: ~p~n", [Seq, Res]),
    Res.

greedy(S, E, Keymap) ->
    {SX, SY} = maps:get(S, Keymap),
    {EX, EY} = maps:get(E, Keymap),
    YSeq = case EY - SY of
               0 ->
                   "";
               PY when PY > 0 ->
                   lists:duplicate(PY, $v);
               NY when NY < 0 ->
                   lists:duplicate(-NY, $^)
           end,
    XSeq = case EX - SX of
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
                            {IX, IY} = maps:get(?INVALID, Keymap),
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
    case {XSeq, YSeq} of
        {"", _} ->
            [YSeq];
        {_, ""} ->
            [XSeq];
        _ ->
            lists:filter(F, [XSeq ++ YSeq, YSeq ++ XSeq])
    end.

d($^) -> { 0, -1};
d($v) -> { 0,  1};
d($<) -> {-1,  0};
d($>) -> { 1,  0}.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
