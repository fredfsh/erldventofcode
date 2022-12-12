-module(aoc_2022_12_1).

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
            array:from_list(X)
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

fin(X) ->
    {Start, End, Heights} = parse(X),
    floodfill(Start, End, Heights).

parse(Arr) ->
    {SX, SY, Arr2} = replace($S, $a, Arr),
    {EX, EY, Arr3} = replace($E, $z, Arr2),
    {{SX, SY}, {EX, EY}, Arr3}.

replace(From, To, Arr) ->
    F = fun(_, Row, {X, Y, Acc}) ->
                {X, Y, array:set(array:size(Acc), Row, Acc)};
           (Y, Row, Acc) ->
                case replace_elem(From, To, Row) of
                    {X, NRow} ->
                        {X, Y, array:set(array:size(Acc), NRow, Acc)};
                    _ ->
                        array:set(array:size(Acc), Row, Acc)
                end
        end,
    array:foldl(F, array:new(), Arr).

replace_elem(From, To, Arr) ->
    F = fun(I, Val, Acc) when Val =:= From ->
                {I, array:set(array:size(Acc), To, Acc)};
           (_, Val, {X, Acc}) ->
                {X, array:set(array:size(Acc), Val, Acc)};
           (_, Val, Acc) ->
                array:set(array:size(Acc), Val, Acc)
        end,
    array:foldl(F, array:new(), Arr).

floodfill(Start, End, Heights) ->
    Q = queue:from_list([{End, 0}]),
    Visited = sets:from_list([End]),
    floodfill_impl(Q, Visited, Start, Heights).

-define(D, [{-1, 0}, {1, 0}, {0, 1}, {0, -1}]).

-define(h(X, Y), array:get(X, array:get(Y, Heights))).

floodfill_impl(Q, Visited, Start, Heights) ->
    {{value, {{X, Y}, N}}, Q2} = queue:out(Q),
    Rows = array:size(Heights),
    Cols = array:size(array:get(0, Heights)),
    F = fun({DX, DY}, {QAcc, VAcc} = Acc) ->
                {NX, NY} = {X + DX, Y + DY},
                case NX >= 0 andalso NX < Cols andalso
                    NY >= 0 andalso NY < Rows andalso
                    ?h(X, Y) =< ?h(NX, NY) + 1 andalso
                    (not sets:is_element({NX, NY}, VAcc)) of
                    true when {NX, NY} =:= Start ->
                        N + 1;
                    true ->
                        {queue:in({{NX, NY}, N + 1}, QAcc),
                         sets:add_element({NX, NY}, VAcc)};
                    false ->
                        Acc
                end;
           (_, Acc) ->
                Acc
        end,
    case lists:foldl(F, {Q2, Visited}, ?D) of
        Res when is_integer(Res) ->
            Res;
        {NQ, NVisited} ->
            floodfill_impl(NQ, NVisited, Start, Heights)
    end.
