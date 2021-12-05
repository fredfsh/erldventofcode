-module(aoc_2019_22_2).

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
    case io:fread("", "~a") of
        eof ->
            eof;
        {ok, [cut]} ->
            {ok, [X]} = io:fread("", "~d"),
            {cut, X};
        {ok, [deal]} ->
            case io:fread("", "~a") of
                {ok, [with]} ->
                    {ok, [_, X]} = io:fread("", "~s ~d"),
                    {deal, X};
                {ok, [into]} ->
                    io:get_line(""),
                    reverse
            end
    end.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

-define(N, 119315717514047).
-define(TIMES, 101741582076661).
-define(TARGET, 2020).

%% https://codeforces.com/blog/entry/72593
fin(X) ->
    LCF = lcf(X),
    {A, B} = pow(LCF, ?TIMES),
    (A * ?TARGET + B) rem ?N.

lcf(Moves) ->
    F = fun(Move, Acc) ->
                compose(Acc, lcf_impl(Move))
        end,
    lists:foldl(F, {1, 0}, Moves).

lcf_impl(reverse) ->
    {?N - 1, ?N - 1};
lcf_impl({cut, X}) ->
    {1, X};
%% F(i) = X * i (mod N)
%% F(i) + S * N = X * i (mod N)
%% i = (S * N + F(i)) / X (mod N)
%% F^(i) = (S * N + i) / X (mod N)
%% Fermat's little theorem: a^P = a (mod P) => a^(P-1) = 1 (mod P)
%% F^(i) = (S * N + i) * X^(N - 2) (mod N) = X^(N - 2) * i (mod N)
lcf_impl({deal, X}) ->
    {deal(X), 0}.

compose({A, B}, {C, D}) ->
    {A * C rem ?N, (B * C + D) rem ?N}.

deal(X) ->
    deal_impl(1, X rem ?N, ?N - 2).

deal_impl(P, _, 0) ->
    P;
deal_impl(P, X, N) when N band 1 =:= 0 ->
    deal_impl(P, X * X rem ?N, N bsr 1);
deal_impl(P, X, N) when N band 1 =:= 1 ->
    deal_impl(P * X rem ?N, X * X rem ?N, N bsr 1).

pow(LCF, N) ->
    pow_impl({1, 0}, LCF, N).

pow_impl(P, _, 0) ->
    P;
pow_impl(P, X, N) when N band 1 =:= 0 ->
    pow_impl(P, compose(X, X), N bsr 1);
pow_impl(P, X, N) when N band 1 =:= 1 ->
    pow_impl(compose(P, X), compose(X, X), N bsr 1).
