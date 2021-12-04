-module(aoc_2021_4_2).

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
    case io:get_line("") of
        eof ->
            eof;
        _ ->
            input_board()
    end.

-record(board, {map,
                marked = sets:new(),
                row_marks = maps:new(),
                col_marks = maps:new()}).

-define(N, 5).

input_board() ->
    F = fun(Y, FAcc) ->
                G = fun(X, GAcc) ->
                            {ok, [Z]} = io:fread("", "~d"),
                            maps:put(Z, {X, Y}, GAcc)
                    end,
                lists:foldl(G, FAcc, lists:seq(1, ?N))
        end,
    #board{map = lists:foldl(F, maps:new(), lists:seq(1, ?N))}.

ini() ->
    {ok, [L]} = io:fread("", "~s"),
    Stream = [list_to_integer(X) || X <- string:split(L, ",", all)],
    {Stream, []}.

do(X) ->
    X.

acc({Stream, Boards}, X) ->
    {Stream, [X | Boards]}.

fin({Stream, Boards}) ->
    game(Stream, Boards).

game([H | T], Boards) ->
    F = fun(Board, {ScoreAcc, BoardsAcc}) ->
                case mark(H, Board) of
                    #board{} = NewBoard ->
                        {ScoreAcc, [NewBoard | BoardsAcc]};
                    Score ->
                        {Score, BoardsAcc}
                end
        end,
    case lists:foldl(F, {undefined, []}, Boards) of
        {Score, []} ->
            Score;
        {_, NewBoards} ->
            game(T, NewBoards)
    end.

mark(Z, #board{map=Map, marked=Marked, row_marks=RM, col_marks=CM} = Board) ->
    case maps:get(Z, Map, undefined) of
        undefined ->
            Board;
        {X, Y} ->
            NMarked = sets:add_element(Z, Marked),
            case {maps:get(Y, RM, 0) + 1,
                  maps:get(X, CM, 0) + 1} of
                {?N, _} ->
                    score(Z, Board#board{marked = NMarked});
                {_, ?N} ->
                    score(Z, Board#board{marked = NMarked});
                {RC, CC} ->
                    Board#board{marked = NMarked,
                                row_marks = maps:put(Y, RC, RM),
                                col_marks = maps:put(X, CC, CM)}
            end
    end.

score(Z, #board{map = Map, marked = Marked}) ->
    F = fun(X) -> not sets:is_element(X, Marked) end,
    lists:sum(lists:filter(F, maps:keys(Map))) * Z.
