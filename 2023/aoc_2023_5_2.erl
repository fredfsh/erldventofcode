-module(aoc_2023_5_2).

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
    S = string:trim(L),
    Parts = string:split(S, " ", all),
    Xs = [list_to_integer(X) || X <- Parts],
    seeds(Xs).

seeds(L) ->
    seeds_impl([], L).

seeds_impl(Acc, []) ->
    Acc;
seeds_impl(Acc, [Start, Range | T]) ->
    seeds_impl([{Start, Range} | Acc], T).

input_maps() ->
    input_maps_impl({maps:new(), maps:new()}).

input_maps_impl({Maps, RMaps} = Acc) ->
    case io:fread("", " ~s map:") of
        eof ->
            Acc;
        {ok, [S]} ->
            [FromStr, ToStr] = string:split(string:trim(S), "-to-"),
            From = list_to_atom(FromStr),
            To = list_to_atom(ToStr),
            Ranges = input_ranges(),
            NewAcc = {maps:put(From, {To, lists:keysort(1, Ranges)}, Maps),
                      maps:put(To, {From, lists:keysort(2, Ranges)}, RMaps)},
            input_maps_impl(NewAcc)
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

ini() ->
    0.

do({Seeds, {Maps, RMaps}}) ->
    F = fun(To, {_, Ranges}, FAcc) ->
                G = fun({Src, Dst, _}, GAcc) ->
                            L = [Src - 1, Src, Src + 1, Dst - 1, Dst, Dst + 1],
                            H = fun(X) -> seed(To, X, RMaps, Seeds) end,
                            L2 = lists:filter(H, L),
                            L3 = [location(To, X, Maps) || X <- L2],
                            lists:min([GAcc | L3])
                    end,
                lists:foldl(G, FAcc, Ranges)
        end,
    maps:fold(F, undefined, Maps).

seed(seed, Seed, _, Seeds) ->
    F = fun({Start, Range}) ->
                Seed >= Start andalso Seed < Start + Range
        end,
    lists:any(F, Seeds);
seed(To, X, RMaps, Seeds) ->
    {From, Ranges} = maps:get(To, RMaps),
    Src =
        case X < element(2, hd(Ranges)) of
            true ->
                X;
            false ->
                src(X, Ranges)
        end,
    seed(From, Src, RMaps, Seeds).

src(X, Ranges) ->
    Arr = array:from_list(Ranges),
    binary_search(0, array:size(Arr), 2, X, Arr).

%% invariant: Left < Right, Arr[Left] <= X < Arr[Right]
binary_search(Left, Right, Key, X, Arr) ->
    case Left + 1 of
        Right ->
            case array:get(Left, Arr) of
                {_, _, start} = Elem ->
                    X - element(Key, Elem) + element(3 - Key, Elem);
                {_, _, stop} = Elem when element(Key, Elem) =:= X ->
                    element(3 - Key, Elem);
                {_, _, stop} ->
                    X
            end;
        _ ->
            Mid = Left + (Right - Left) div 2,
            case element(Key, array:get(Mid, Arr)) > X of
                true ->
                    binary_search(Left, Mid, Key, X, Arr);
                _ ->
                    binary_search(Mid, Right, Key, X, Arr)
            end
    end.

location(location, Location, _) ->
    Location;
location(From, X, Maps) ->
    {To, Ranges} = maps:get(From, Maps),
    Dst =
        case X < element(1, hd(Ranges)) of
            true ->
                X;
            false ->
                dst(X, Ranges)
        end,
    location(To, Dst, Maps).

dst(X, Ranges) ->
    Arr = array:from_list(Ranges),
    binary_search(0, array:size(Arr), 1, X, Arr).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
