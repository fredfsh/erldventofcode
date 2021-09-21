-module(aoc_2016_7_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(0).

run_impl(Acc) ->
    case input() of
        eof ->
            Acc;
        X ->
            run_impl(Acc + do(X))
    end.

input() ->
    case io:fread("", "~s") of
        eof ->
            eof;
        {ok, [X]} ->
            X
    end.

do(X) ->
    ABs = aba(X),
    bab(X, ABs).

aba(X) ->
    aba_impl(X, sets:new(), true, $#, $#).

aba_impl([], Acc, _, _, _) ->
    Acc;
aba_impl([$[ | T], Acc, true, _, _) ->
    aba_impl(T, Acc, false, $#, $#);
aba_impl([$] | T], Acc, false, _, _) ->
    aba_impl(T, Acc, true, $#, $#);
aba_impl([_ | T], Acc, false, _, _) ->
    aba_impl(T, Acc, false, $#, $#);
aba_impl([A | T], Acc, true, B, A) ->
    aba_impl(T, sets:add_element({A, B}, Acc), true, A, B);
aba_impl([X | T], Acc, true, P1, _P2) ->
    aba_impl(T, Acc, true, X, P1).

bab(X, ABs) ->
    bab_impl(X, false, $#, $#, ABs).

bab_impl([], _, _, _, _) ->
    0;
bab_impl([$[ | T], false, _, _, ABs) ->
    bab_impl(T, true, $#, $#, ABs);
bab_impl([$] | T], true, _, _, ABs) ->
    bab_impl(T, false, $#, $#, ABs);
bab_impl([_ | T], false, _, _, ABs) ->
    bab_impl(T, false, $#, $#, ABs);
bab_impl([B | T], true, A, B, ABs) ->
    case sets:is_element({A, B}, ABs) of
        true ->
            1;
        _ ->
            bab_impl(T, true, B, A, ABs)
    end;
bab_impl([X | T], true, P1, _P2, ABs) ->
    bab_impl(T, true, X, P1, ABs).
