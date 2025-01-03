-module(aoc_2024_17_2).

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

-define(FMT, "Register A: ~d Register B: ~d Register C: ~d Program: ~s").

-record(cpu, {a, b, c, code}).

input() ->
    case io:fread("", ?FMT) of
        eof ->
            eof;
        {ok, [A, B, C, Txt]} ->
            L = string:lexemes(Txt, ","),
            Prog = array:from_list([list_to_integer(S) || S <- L]),
            #cpu{a = A, b = B, c = C, code = Prog}
    end.

ini() ->
    0.

%% Program: 2,4,1,7,7,5,0,3,4,4,1,7,5,5,3,0
%%
%% IP | Code | Instruction | Comment
%% ---+------+-------------+--------
%%  0 | 2, 4 |      bst, 4 | b = A & 7
%%  1 | 1, 7 |      bxl, 7 | b = B bxor 7 = (A&7) bxor 7 = ~(A&7)
%%  2 | 7, 5 |      cdv, 5 | c = trunc(A / 2^B) = A >> B = A >> ~(A&7)
%%  3 | 0, 3 |      adv, 3 | a = trunc(A / 2^3) = A >> 3
%%  4 | 4, 4 |      bxc, 4 | b = B bxor C = [~(A&7)] bxor [A >> ~(A&7)]
%%  5 | 1, 7 |      bxl, 7 | b = B bxor 7 = [~(A&7)] bxor [A >> ~(A&7)] bxor 7 = A&7 bxor [A >> ~(A&7)]
%%  6 | 5, 5 |      out, 5 | out: B & 7 = A&7 bxor [A >> ~(A&7)] & 7
%%  7 | 3, 0 |      jnz, 0 | jump to 0 if (A bsr 3 =/= 0)
%%
%%          A | A&7 | ~(A&7) | A >> ~(A&7) | A&7 ^ [A >> ~(A&7)] & 7
%% -----------+-----+--------+-------------+------------------------
%%        111 | 111 |    000 |         111 |                     000
%%       x110 | 110 |    001 |         x11 |                  (~x)01
%%      yx101 | 101 |    010 |         yx1 |                  (~y)x0
%%     zyx100 | 100 |    011 |         zyx |                  (~z)yx
%%    zyx?011 | 011 |    100 |         zyx |               z(~y)(~x)
%%   zyx??010 | 010 |    101 |         zyx |                  z(~y)x
%%  zyx???001 | 001 |    110 |         zyx |                  zy(~x)
%% zyx????000 | 000 |    111 |         zyx |                     zyx
%%
do(#cpu{code = Code}) ->
    dfs(maps:new(), array:to_list(Code), 0).

-define(MB(N, X), (maps:get(N, Bits, X) =:= X)).
-define(MB(N, X, Y, Z), ?MB(N, X) andalso ?MB(N + 1, Y) andalso ?MB(N + 2, Z)).
-define(MB(N, X, Y, Z, W), ?MB(N, X, Y, Z) andalso ?MB(N + 3, W)).
-define(MB(N, X, Y, Z, W, V), ?MB(N, X, Y, Z, W) andalso ?MB(N + 4, V)).

dfs(Bits, [], _) ->
    F = fun(K, V, Acc) ->
                Acc bor (V bsl K)
        end,
    maps:fold(F, 0, Bits);
dfs(Bits, [H | T], N) ->
    Acc = infinity,

    Acc7 =
        case H =:= 0 andalso ?MB(N, 1, 1, 1) of
            true ->
                M7 = #{N => 1, N + 1 => 1, N + 2 => 1},
                Bits7 = maps:merge(Bits, M7),
                Res7 = dfs(Bits7, T, N + 3),
                min(Res7, Acc);
            false ->
                Acc
        end,

    X6 = ((H bsr 2) band 1) bxor 1,
    Acc6 =
        case H band 3 =:= 1 andalso ?MB(N, 0, 1, 1, X6) of
            true ->
                M6 = #{N => 0, N + 1 => 1, N + 2 => 1, N + 3 => X6},
                Bits6 = maps:merge(Bits, M6),
                Res6 = dfs(Bits6, T, N + 3),
                min(Res6, Acc7);
            false ->
                Acc7
        end,

    X5 = (H bsr 1) band 1,
    Y5 = ((H bsr 2) band 1) bxor 1,
    Acc5 =
        case H band 1 =:= 0 andalso ?MB(N, 1, 0, 1, X5, Y5) of
            true ->
                M5 = #{N => 1, N+1 => 0, N+2 => 1, N+3 => X5, N+4 => Y5},
                Bits5 = maps:merge(Bits, M5),
                Res5 = dfs(Bits5, T, N + 3),
                min(Res5, Acc6);
            false ->
                Acc6
        end,

    X4 = H band 1,
    Y4 = (H bsr 1) band 1,
    Z4 = ((H bsr 2) band 1) bxor 1,
    Acc4 =
        case ?MB(N, 0, 0, 1) andalso ?MB(N + 3, X4, Y4, Z4) of
            true ->
                M4 = #{N=>0, N+1=>0, N+2=>1, N+3=>X4, N+4=>Y4, N+5=>Z4},
                Bits4 = maps:merge(Bits, M4),
                Res4 = dfs(Bits4, T, N + 3),
                min(Res4, Acc5);
            false ->
                Acc5
        end,

    X3 = (H band 1) bxor 1,
    Y3 = ((H bsr 1) band 1) bxor 1,
    Z3 = (H bsr 2) band 1,
    Acc3 =
        case ?MB(N, 1, 1, 0) andalso ?MB(N + 4, X3, Y3, Z3) of
            true ->
                M3 = #{N=>1, N+1=>1, N+2=>0, N+4=>X3, N+5=>Y3, N+6=>Z3},
                Bits3 = maps:merge(Bits, M3),
                Res3 = dfs(Bits3, T, N + 3),
                min(Res3, Acc4);
            false ->
                Acc4
        end,

    X2 = H band 1,
    Y2 = ((H bsr 1) band 1) bxor 1,
    Z2 = (H bsr 2) band 1,
    Acc2 =
        case ?MB(N, 0, 1, 0) andalso ?MB(N + 5, X2, Y2, Z2) of
            true ->
                M2 = #{N=>0, N+1=>1, N+2=>0, N+5=>X2, N+6=>Y2, N+7=>Z2},
                Bits2 = maps:merge(Bits, M2),
                Res2 = dfs(Bits2, T, N + 3),
                min(Res2, Acc3);
            false ->
                Acc3
        end,

    X1 = (H band 1) bxor 1,
    Y1 = (H bsr 1) band 1,
    Z1 = (H bsr 2) band 1,
    Acc1 =
        case ?MB(N, 1, 0, 0) andalso ?MB(N + 6, X1, Y1, Z1) of
            true ->
                M1 = #{N=>1, N+1=>0, N+2=>0, N+6=>X1, N+7=>Y1, N+8=>Z1},
                Bits1 = maps:merge(Bits, M1),
                Res1 = dfs(Bits1, T, N + 3),
                min(Res1, Acc2);
            false ->
                Acc2
        end,

    X0 = H band 1,
    Y0 = (H bsr 1) band 1,
    Z0 = (H bsr 2) band 1,
    Acc0 =
        case ?MB(N, 0, 0, 0) andalso ?MB(N + 7, X0, Y0, Z0) of
            true ->
                M0 = #{N=>0, N+1=>0, N+2=>0, N+7=>X0, N+8=>Y0, N+9=>Z0},
                Bits0 = maps:merge(Bits, M0),
                Res0 = dfs(Bits0, T, N + 3),
                min(Res0, Acc1);
            false ->
                Acc1
        end,

    Acc0.

acc(Acc, X) ->
    Acc + X.

fin(X) ->
    X.
