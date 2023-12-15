-module(aoc_2023_15_2).

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
        {ok, [L]} ->
            string:split(string:trim(L), ",", all)
    end.

ini() ->
    0.

do(L) ->
    F = fun(Step, Acc) ->
                case lists:last(Step) of
                    $- ->
                        Label = lists:droplast(Step),
                        Index = hash(Label),
                        Box = array:get(Index, Acc),
                        NewBox = lists:keydelete(Label, 1, Box),
                        array:set(Index, NewBox, Acc);
                    _ ->
                        [Label, NStr] = string:split(Step, "="),
                        FocalLength = list_to_integer(NStr),
                        Len = {Label, FocalLength},
                        Index = hash(Label),
                        Box = array:get(Index, Acc),
                        NewBox = case lists:keymember(Label, 1, Box) of
                                     true ->
                                         lists:keyreplace(Label, 1, Box, Len);
                                     false ->
                                         Box ++ [Len]
                                 end,
                        array:set(Index, NewBox, Acc)
                end
        end,
    Boxes = lists:foldl(F, array:new([256, {default, []}]), L),
    focusing_power(Boxes).

focusing_power(Boxes) ->
    F = fun(BI, Box, FAcc) ->
                G = fun({_, FocalLength}, {GAcc, SI}) ->
                            {GAcc + (BI + 1) * SI * FocalLength, SI + 1}
                    end,
                {GRes, _} = lists:foldl(G, {FAcc, 1}, Box),
                GRes
        end,
    array:foldl(F, 0, Boxes).

hash(S) ->
    hash_impl(0, S).

hash_impl(Acc, []) ->
    Acc;
hash_impl(Acc, [H | T]) ->
    hash_impl((Acc + H) * 17 rem 256, T).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
