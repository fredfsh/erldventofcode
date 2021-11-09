-module(aoc_2017_23_2).

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
    case io:fread("", "~a ~s ~s") of
        eof ->
            eof;
        {ok, [Cmd, X, Y]} ->
            {Cmd, reg_or_int(X), reg_or_int(Y)}
    end.

reg_or_int(S) ->
    case string:to_integer(S) of
        {error, no_integer} ->
            list_to_atom(S);
        {N, _} ->
            N
    end.

ini() ->
    array:new().

do(X) ->
    X.

acc(Acc, X) ->
    array:set(array:size(Acc), X, Acc).

%% h holds number of composites in the sequence defined by the constants
%%
%%        | set b 108100
%%        | set c 125100

%% B:   8 | set f 1
%%      9 | set d 2

%% E:  10 | set e 2

%% D:  11 | set g d
%%     12 | mul g e
%%     13 | sub g b
%%     14 | jnz g *C*
%%     15 | set f 0

%% C:  16 | add e 1
%%     17 | set g e
%%     18 | sub g b
%%     19 | jnz g *D*
%%     20 | add d 1
%%     21 | set g d
%%     22 | sub g b
%%     23 | jnz g *E*
%%     24 | jnz f *F*
%%     25 | add h 1

%% F:  26 | set g b
%%     27 | sub g c
%%     28 | jnz g *G*
%%     29 | EXIT

%% G:  30 | add b 17
%%     31 | goto *B*

fin(X) ->
    {set, b, N1} = array:get(0, X),
    {mul, b, N2} = array:get(4, X),
    {sub, b, N3} = array:get(5, X),
    {sub, c, N4} = array:get(7, X),
    {sub, b, N5} = array:get(array:size(X) - 2, X),
    Start = N1 * N2 - N3,
    L = lists:seq(Start, Start - N4, -N5),
    F = fun(N, Acc) ->
                case is_prime(N) of
                    false ->
                        Acc + 1;
                    true ->
                        Acc
                end
        end,
    lists:foldl(F, 0, L).

is_prime(X) ->
    lists:all(fun(N) -> X rem N =/= 0 end, lists:seq(2, X - 1)).
