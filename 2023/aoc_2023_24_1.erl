-module(aoc_2023_24_1).

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
    case io:fread("", "~s ~s ~s @ ~s ~s ~s") of
        eof ->
            eof;
        {ok, L} ->
            [list_to_integer(string:trim(S, trailing, ",")) || S <- L]
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, [PX, PY, _, VX, VY, _]) ->
    [{PX, PY, VX, VY} | Acc].

fin(L) ->
    L2 = [overlap(H) || H <- L],
    %%io:format("~p~n", [L2]),
    L3 = [undefined || X <- L2, Y <- L2, intersect(X, Y)],
    length(L3) div 2.

-define(MIN, 200000000000000).
-define(MAX, 400000000000000).

overlap({PX, PY, _, _} = Halfline) ->
    Lines = [{x, ?MIN}, {x, ?MAX}, {y, ?MIN}, {y, ?MAX}],
    Points = [P || P <- [overlap(Halfline, L) || L <- Lines], P =/= undefined],
    Res = sets:to_list(sets:from_list(Points)),
    case PX > ?MIN andalso PX < ?MAX andalso PY > ?MIN andalso PY < ?MAX of
        true ->
            Res ++ [{{PX, 1}, {PY, 1}}];
        false ->
            Res
    end.

overlap({PX, PY, VX, VY}, {x, X}) ->
    overlap_impl(PX, PY, VX, VY, X);
overlap({PX, PY, VX, VY}, {y, Y}) ->
    case overlap_impl(PY, PX, VY, VX, Y) of
        undefined ->
            undefined;
        {OY, OX} ->
            {OX, OY}
    end.

overlap_impl(X, PY, _, _, X) when PY >= ?MIN, PY =< ?MAX ->
    {{X, 1}, {PY, 1}};
overlap_impl(X, _, _, _, X) ->
    undefined;
overlap_impl(PX, _, VX, _, X) when PX < X, VX =< 0 ->
    undefined;
overlap_impl(PX, _, VX, _, X) when PX > X, VX >= 0 ->
    undefined;
overlap_impl(PX, PY, VX, VY, X) ->
    %% Y = PY + (X - PX) / VX * VY,
    case (X - PX) * VY + VX * PY of
        N when VX > 0, N >= ?MIN * VX, N =< ?MAX * VX ->
            {{X, 1}, simplify(N, VX)};
        N when VX < 0, N >= ?MAX * VX, N =< ?MIN * VX ->
            {{X, 1}, simplify(-N, -VX)};
        _ ->
            undefined
    end.

simplify(Numerator, Denominator) ->
    GCD = gcd(Numerator, Denominator),
    {Numerator div GCD, Denominator div GCD}.

gcd(A, B) when A < B ->
    gcd(B, A);
gcd(A, B) when A rem B =:= 0 ->
    B;
gcd(A, B) ->
    gcd(B, A rem B).

intersect(PAs, PBs) when length(PBs) > length(PAs) ->
    intersect(PBs, PAs);
intersect(_, []) ->
    false;
intersect([Point], [Point]) ->
    true;
intersect([_], [_]) ->
    false;
intersect([PB, _], [PB]) ->
    true;
intersect([_, PB], [PB]) ->
    true;
intersect([_, _], [_]) ->
    false;
intersect([PA, PB], [PA, PB]) ->
    false;
intersect([PA1, PA2], [PB1, PB2]) ->
    intersect_impl(PA1, PA2, PB1, PB2).

%% https://bryceboe.com/2006/10/23/line-segment-intersection-algorithm/
intersect_impl(PA1, PA2, PB1, PB2) ->
    counterclockwise(PA1, PB1, PB2) =/= counterclockwise(PA2, PB1, PB2) andalso
        counterclockwise(PB1, PA1, PA2) =/= counterclockwise(PB2, PA1, PA2).

counterclockwise({{AXN, AXD}, {AYN, AYD}},
                 {{BXN, BXD}, {BYN, BYD}},
                 {{CXN, CXD}, {CYN, CYD}}) ->
    BYD * CXD * (CYN * AYD - CYD * AYN) * (BXN * AXD - BXD * AXN) >
        CYD * BXD * (BYN * AYD - BYD * AYN) * (CXN * AXD - CXD * AXN).
