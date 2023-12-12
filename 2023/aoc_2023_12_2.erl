-module(aoc_2023_12_2).

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
    case io:fread("", "~s ~s") of
        eof ->
            eof;
        {ok, [AStr, BStr]} ->
            Ss = string:split(string:trim(BStr), ",", all),
            {array:from_list(string:trim(AStr)),
             [list_to_integer(S) || S <- Ss]}
    end.

-define(TAB, memo).

ini() ->
    ets:new(?TAB, [named_table]),
    0.

do(Folded) ->
    {S, L} = unfold(Folded),
    %%io:format("~p~n~p~n", [S, L]),
    ets:delete_all_objects(?TAB),
    do_impl(0, L, 0, array:size(S), S).

-define(FOLDS, 5).

unfold({Sin, Lin}) ->
    S0 = array:to_list(Sin),
    S1 = lists:duplicate(?FOLDS, S0),
    S2 = lists:join($?, S1),
    S3 = lists:flatten(S2),
    Sout = array:from_list(S3),

    L0 = lists:duplicate(?FOLDS, Lin),
    Lout = lists:append(L0),

    {Sout, Lout}.

-define(s, array:get(IS, S)).
-define(s(I), array:get(I, S)).

do_impl(0, [], LS, LS, _) ->
    1;
do_impl(0, _, LS, LS, _) ->
    0;
do_impl(Acc, [Acc], LS, LS, _) ->
    1;
do_impl(_, _, LS, LS, _) ->
    0;
do_impl(0, [], IS, LS, S) ->
    case ?s of
        $# ->
            0;
        _ ->
            do_impl(0, [], IS + 1, LS, S)
    end;
do_impl(_, [], _, _, _) ->
    0;
do_impl(Acc, L, IS, LS, S) ->
    Key = {Acc, length(L), IS},
    case ets:lookup(?TAB, Key) of
        [] ->
            Res = case ?s of
                      $. when Acc =:= 0 ->
                          do_impl(0, L, IS + 1, LS, S);
                      $. when hd(L) =:= Acc ->
                          do_impl(0, tl(L), IS + 1, LS, S);
                      $. ->
                          0;
                      $# when Acc + 1 > hd(L) ->
                          0;
                      $# when Acc + 1 =:= hd(L) andalso IS =:= LS - 1 ->
                          do_impl(0, tl(L), LS, LS, S);
                      $# when Acc + 1 =:= hd(L) ->
                          case ?s(IS + 1) of
                              $# ->
                                  0;
                              _ ->
                                  do_impl(0, tl(L), IS + 2, LS, S)
                          end;
                      $# ->
                          do_impl(Acc + 1, L, IS + 1, LS, S);
                      $? ->
                          %% operational (.)
                          Case1 = case Acc of
                                      0 ->
                                          %%io:format("do_impl(~p,~p,~p) case1 => do_impl(0,~p,~p)~n", [Acc, length(L), IS, length(L), IS + 1]),
                                          do_impl(0, L, IS + 1, LS, S);
                                      X when hd(L) =:= X ->
                                          %%io:format("do_impl(~p,~p,~p) case1 => do_impl(0,~p,~p)~n", [Acc, length(L), IS, length(tl(L)), IS + 1]),
                                          do_impl(0, tl(L), IS + 1, LS, S);
                                      _ ->
                                          %%io:format("do_impl(~p,~p,~p) case1 => 0~n", [Acc, length(L), IS]),
                                          0
                                  end,
                          %% damanged (#)
                          Case2 =  case Acc + 1 - hd(L) of
                                       0 when IS =:= LS - 1 ->
                                           do_impl(0, tl(L), LS, LS, S);
                                       0 ->
                                           case ?s(IS + 1) of
                                               $# ->
                                                   0;
                                               _ ->
                                                   do_impl(0, tl(L), IS + 2, LS, S)
                                           end;
                                       Pos when Pos > 0 ->
                                           0;
                                       Neg when Neg < 0 ->
                                           do_impl(Acc + 1, L, IS + 1, LS, S)
                                   end,
                          %%io:format("do_impl(~p,~p,~p) cases (~p,~p)~n", [Acc, length(L), IS, Case1, Case2]),
                          Case1 + Case2
                  end,
            ets:insert_new(?TAB, [{Key, Res}]),
            %%io:format("do_impl(~p,~p,~p) => ~p~n", [Acc, length(L), IS, Res]),
            Res;
        [{_, Val}] ->
            %%io:format("do_impl(~p,~p,~p) memo => ~p~n", [Acc, length(L), IS, Val]),
            Val
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
