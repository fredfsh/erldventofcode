-module(aoc_2025_5_1).

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
        Line ->
            %%io:format("line: ~w~n", [Line]),
            case string:split(string:trim(Line), "-") of
                [[]] ->
                    input();
                [ID] ->
                    list_to_integer(ID);
                [From, To] ->
                    {list_to_integer(From), list_to_integer(To)}
            end
    end.

ini() ->
    {[], []}.

do(X) ->
    X.

acc({Ranges, IDs}, ID) when is_integer(ID) ->
    {Ranges, [ID | IDs]};
acc({Ranges, IDs}, Range) ->
    {[Range | Ranges], IDs}.

fin({Ranges, IDs}) ->
    %%io:format("~w ~w~n", [Ranges, IDs]),
    F = fun(ID) ->
                G = fun({From, To}) -> ID >= From andalso ID =< To end,
                lists:any(G, Ranges)
        end,
    length(lists:filter(F, IDs)).
