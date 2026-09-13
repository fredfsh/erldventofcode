-module(aoc_2025_7_1).

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
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    [Header | Graph] = lists:reverse(X),
    Beam = length(hd(string:lexemes(array:to_list(Header), "S"))),
    splits(0, sets:from_list([Beam]), Graph).

splits(Acc, _, [_]) ->
    Acc;
splits(Acc, Beams, [_, H | T]) ->
    F = fun(Beam, {SAcc, NAcc}) ->
                case array:get(Beam, H) of
                    $^ ->
                        NAcc1 = sets:add_element(Beam - 1, NAcc),
                        NAcc2 = sets:add_element(Beam + 1, NAcc1),
                        {SAcc + 1, NAcc2};
                    $. ->
                        {SAcc, sets:add_element(Beam, NAcc)}
                end
        end,
    {Splits, NewBeams} = sets:fold(F, {0, sets:new()}, Beams),
    splits(Acc + Splits, NewBeams, T).
