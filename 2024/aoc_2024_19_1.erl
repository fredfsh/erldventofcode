-module(aoc_2024_19_1).

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
    L = io:get_line(""),
    {string:lexemes(L, ", \n"), []}.

do(X) ->
    X.

acc({Towels, Acc}, X) ->
    {Towels, [X | Acc]}.

fin({Towels, Designs}) ->
    length([undefined || D <- Designs, design(D, Towels)]).

design(Design, Towels) ->
    Q = queue:from_list([0]),
    Seen = sets:from_list([0]),
    bfs(Q, Seen, array:from_list(Design), sets:from_list(Towels)).

%% Acc: 0 1 2 3 4 5
%% Str: |a|b|c|d|e|
%% Idx:  0 1 2 3 4
bfs(Q, Seen, Design, Towels) ->
%%    io:format("bfs ~s: ~p, ~p~n", [array:to_list(Design), queue:to_list(Q), sets:to_list(Seen)]),
    N = array:size(Design),
    case queue:out(Q) of
        {empty, _} ->
            false;
        {{value, I}, _} when I =:= N ->
            true;
        {{value, I}, Q2} ->
            F = fun(J, {QAcc, SeenAcc, SAcc}) ->
                        S = SAcc ++ [array:get(J - 1, Design)],
                        case sets:is_element(S, Towels)
                            andalso not sets:is_element(J, SeenAcc) of
                            true ->
                                {queue:in(J, QAcc),
                                 sets:add_element(J, SeenAcc),
                                 S};
                            false ->
                                {QAcc, SeenAcc, S}
                        end
                end,
            {NQ, NSeen, _} = lists:foldl(F, {Q2, Seen, []}, lists:seq(I+1, N)),
            bfs(NQ, NSeen, Design, Towels)
    end.
