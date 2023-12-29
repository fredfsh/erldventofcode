-module(aoc_2023_18_2).

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
    case io:fread("", "~a ~d (#~5c~c)") of
        eof ->
            eof;
        {ok, [_, _, LenStr, [DirChar]]} ->
            {dir(DirChar), list_to_integer(LenStr, 16)}
    end.

dir($0) -> right;
dir($1) -> down;
dir($2) -> left;
dir($3) -> up.

ini() ->
    [].

do(X) ->
    X.

acc(Acc, X) ->
    [X | Acc].

fin(X) ->
    {Levels, Verticals} = (dig(lists:reverse(X))),
    area(Levels, Verticals).

dig(Plans) ->
    dig_impl([], [], 0, 0, Plans).

-record(vertical, {x, y, l}).

dig_impl(Levels, Verticals, _, _, []) ->
    F = fun(#vertical{x = XA}, #vertical{x = XB}) ->
                 XA =< XB
         end,
    {lists:usort(Levels), lists:sort(F, Verticals)};
dig_impl(Levels, Verticals, X, Y, [{Dir, Len} | T]) ->
    case line(X, Y, Dir, Len) of
        {NX, NY, Level} when is_integer(Level) ->
            dig_impl([Level | Levels], Verticals, NX, NY, T);
        {NX, NY, Vertical} ->
            dig_impl(Levels, [Vertical | Verticals], NX, NY, T)
    end.

line(X, Y, up, Len) ->
    {X, Y - Len, #vertical{x = X, y = Y - Len, l = Len}};
line(X, Y, down, Len) ->
    {X, Y + Len, #vertical{x = X, y = Y, l = Len}};
line(X, Y, left, Len) ->
    {X - Len, Y, Y};
line(X, Y, right, Len) ->
    {X + Len, Y, Y}.

area([Y | T], Verticals) ->
    Init = process_line(Y, Verticals),
    area_impl(Init, Y, T, Verticals).

process_line(Level, Verticals) ->
    process_line_impl(0, undefined, Verticals, Level).

process_line_impl(Acc, _, [], _) ->
    Acc;
process_line_impl(Acc, undefined, [#vertical{x = X} = Vertical | T], Level) ->
    case intersect(Vertical, Level) of
        none ->
            process_line_impl(Acc, undefined, T, Level);
        Dir ->
            process_line_impl(Acc, {X, Dir}, T, Level)
    end;
process_line_impl(Acc, {X, Dir}, [#vertical{x = XV} = Vertical | T], Level) ->
    case intersect(Vertical, Level) of
        none ->
            process_line_impl(Acc, {X, Dir}, T, Level);
        DirV ->
            case dir_xor(Dir, DirV) of
                none ->
                    process_line_impl(Acc + (XV - X + 1), undefined, T, Level);
                NewDir ->
                    process_line_impl(Acc, {X, NewDir}, T, Level)
            end
    end.

intersect(#vertical{y = Y, l = Len}, Level) when Y > Level; Y + Len < Level ->
    none;
intersect(#vertical{y = Level}, Level) ->
    down;
intersect(#vertical{y = Y, l = Len}, Level) when Y + Len =:= Level ->
    up;
intersect(_, _) ->
    both.

dir_xor(Dir, Dir) ->
    none;
dir_xor(both, up) ->
    down;
dir_xor(both, down) ->
    up;
dir_xor(up, both) ->
    down;
dir_xor(up, down) ->
    both;
dir_xor(down, both) ->
    up;
dir_xor(down, up) ->
    both.

area_impl(Acc, _, [], _) ->
    Acc;
area_impl(Acc, LastY, [Y | T], Verticals) ->
    RangeArea = process_range(LastY, Y, Verticals),
    LineArea = process_line(Y, Verticals),
    area_impl(Acc + RangeArea + LineArea, Y, T, Verticals).

process_range(YS, YE, _) when YE =:= YS + 1 ->
    0;
process_range(YS, YE, Verticals) ->
    LineArea = process_line(YS + 1, Verticals),
    LineArea * (YE - YS - 1).
