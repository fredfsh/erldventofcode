-module(aoc_2024_13_2).

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

-define(FMT, "Button A: X+~d, Y+~d Button B: X+~d, Y+~d Prize: X=~d, Y=~d").

-define(D, 10000000000000).

input() ->
    case io:fread("", ?FMT) of
        eof ->
            eof;
        {ok, [AX, AY, BX, BY, PX, PY]} ->
            {AX, AY, BX, BY, PX + ?D, PY + ?D}
    end.

ini() ->
    0.

%% solve:
%%   AX * a + BX * b = PX
%%   AY * a + BY * b = PY
%%
%%   AX * AY * a + AY * BX * b = AY * PX
%%   AX * AY * a + AX * BY * b = AX * PY
%%
%%   (AY * BX - AX * BY) * b = AY * PX - AX * PY
do({AX, AY, BX, BY, PX, PY}) when AY * BX =/= AX * BY ->
    case (AY * PX - AX * PY) rem (AY * BX - AX * BY) of
        0 ->
            B = (AY * PX - AX * PY) div (AY * BX - AX * BY),
            A = (PX - BX * B) div AX,
            case A * AX + B * BX =:= PX andalso B >= 0 andalso A >= 0 of
                true ->
                    3 * A + B;
                false ->
                    0
            end;
        _ ->
            0
    end.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
