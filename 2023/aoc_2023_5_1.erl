-module(aoc_2023_5_1).

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
    case io:fread("", "seeds: ") of
        eof ->
            eof;
        {ok, []} ->
            Seeds = input_seeds(),
            Maps = input_maps(),
            {Seeds, Maps}
    end.

input_seeds() ->
    L = io:get_line(""),
    L1 = string:trim(L),
    SeedStrs = string:split(L1, " ", all),
    [list_to_integer(S) || S <- SeedStrs].

input_maps() ->
    input_maps_impl(maps:new()).

input_maps_impl(Acc) ->
    case io:fread("", " ~s map:") of
        eof ->
            Acc;
        {ok, [S]} ->
            [FromStr, ToStr] = string:split(string:trim(S), "-to-"),
            From = list_to_atom(FromStr),
            To = list_to_atom(ToStr),
            Ranges = sort_ranges(input_ranges()),
            input_maps_impl(maps:put(From, {To, Ranges}, Acc))
    end.

input_ranges() ->
    input_ranges_impl([]).

input_ranges_impl(Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        "\n" ->
            Acc;
        L ->
            S = string:trim(L),
            Parts = string:split(S, " ", all),
            [Dst, Src, N] = [list_to_integer(X) || X <- Parts],
            NewAcc = [{Src, Dst, start}, {Src+N-1, Dst+N-1, stop} | Acc],
            input_ranges_impl(NewAcc)
    end.

sort_ranges(Ranges) ->
    lists:keysort(1, Ranges).

ini() ->
    0.

do({Seeds, Maps}) ->
    lists:min([location(Seed, Maps) || Seed <- Seeds]).

location(Seed, Maps) ->
    location_impl(seed, Seed, Maps).

location_impl(location, Location, _) ->
    Location;
location_impl(From, X, Maps) ->
    {To, Ranges} = maps:get(From, Maps),
    Y =
        case X < element(1, hd(Ranges)) of
            true ->
                X;
            false ->
                binary_search(X, Ranges)
        end,
    location_impl(To, Y, Maps).

binary_search(X, Ranges) ->
    Arr = array:from_list(Ranges),
    binary_search(0, array:size(Arr), X, Arr).

%% invariant: Arr[Left] <= X < Arr[Right], Left < Right
binary_search(Left, Right, X, Arr) ->
    case Left + 1 of
        Right ->
            case array:get(Left, Arr) of
                {Src, Dst, start} ->
                    X - Src + Dst;
                {X, Dst, stop} ->
                    Dst;
                {_, _, stop} ->
                    X
            end;
        _ ->
            Mid = Left + (Right - Left) div 2,
            case array:get(Mid, Arr) of
                {N, _, _} when X >= N ->
                    binary_search(Mid, Right, X, Arr);
                _ ->
                    binary_search(Left, Mid, X, Arr)
            end
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
