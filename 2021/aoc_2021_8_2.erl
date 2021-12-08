-module(aoc_2021_8_2).

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
        L ->
            [Left, Right] = string:split(string:trim(L, trailing), " | "),
            {string:split(Left, " ", all), string:split(Right, " ", all)}

    end.

ini() ->
    0.

do({L, R}) ->
    Perms = perm(lists:seq($a, $g)),
    Perm = guess(L, Perms),
    num(R, Perm).

perm([]) ->
    [[]];
perm(L) ->
    [[H | T] || H <- L, T <- perm(L -- [H])].

guess(L, Perms) ->
    guess_impl(L, Perms).

guess_impl(L, [H | T]) ->
    case lists:sort([digit(X, H) || X <- L]) =:= lists:seq(0, 9) of
        true ->
            H;
        _ ->
            guess_impl(L, T)
    end.

digit(On, Perm) ->
    Mapping = maps:from_list(lists:zip(lists:seq($a, $g), Perm)),
    Ons = lists:map(fun(X) -> maps:get(X, Mapping) end, On),
    digit(lists:sort(Ons)).

digit([$a, $b, $c,     $e, $f, $g]) -> 0;
digit([        $c,         $f    ]) -> 1;
digit([$a,     $c, $d, $e,     $g]) -> 2;
digit([$a,     $c, $d,     $f, $g]) -> 3;
digit([    $b, $c, $d,     $f    ]) -> 4;
digit([$a, $b,     $d,     $f, $g]) -> 5;
digit([$a, $b,     $d, $e, $f, $g]) -> 6;
digit([$a,     $c,         $f    ]) -> 7;
digit([$a, $b, $c, $d, $e, $f, $g]) -> 8;
digit([$a, $b, $c, $d,     $f, $g]) -> 9;
digit(_) -> -1.

num(R, Perm) ->
    num_impl(0, R, Perm).

num_impl(Acc, [], _) ->
    Acc;
num_impl(Acc, [H | T], Perm) ->
    num_impl(Acc * 10 + digit(H, Perm), T, Perm).

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
