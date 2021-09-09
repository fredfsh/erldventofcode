-module(aoc_2016_4_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl().

run_impl() ->
    case input() of
        eof ->
            "Not found!";
        X ->
            case do(X) of
                {true, ID} ->
                    ID;
                _ ->
                    run_impl()
            end
    end.

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    ID = sector_id(X),
    Name = name(X, ID),
    case string:find(Name, "pole") of
        nomatch ->
            false;
        _ ->
            {true, ID}
    end.

sector_id(X) ->
    sector_id(X, 0).

sector_id([$[ | _], Acc) ->
    Acc;
sector_id([N | T], Acc) when N >= $0, N =< $9 ->
    sector_id(T, Acc * 10 + (N - $0));
sector_id([_ | T], 0) ->
    sector_id(T, 0).

name(X, ID) ->
    name(X, [], ID).

name([$- | T], Acc, ID) ->
    name(T, [32 | Acc], ID);
name([_N | _T], Acc, _ID) when _N >= $0, _N =< $9 ->
    lists:reverse(Acc);
name([X | T], Acc, ID) ->
    name(T, [((X - $a) + ID) rem 26 + $a | Acc], ID).
