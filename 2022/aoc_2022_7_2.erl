-module(aoc_2022_7_2).

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
    case io:fread("", "~s ~s") of
        eof ->
            eof;
        {ok, ["$", "ls"]} ->
            ls;
        {ok, ["$", "cd"]} ->
            {ok, [DirStr]} = io:fread("", " ~s"),
            {cd, list_to_atom(DirStr)};
        {ok, ["dir", DirStr]} ->
            {dir, list_to_atom(DirStr)};
        {ok, [SizeStr, FileStr]} ->
            {file, list_to_integer(SizeStr), list_to_atom(FileStr)}
    end.

ini() ->
    {maps:new(), []}.

do(X) ->
    X.

acc({Files, [_Current | Parent]}, {cd, '..'}) ->
    {Files, Parent};
acc({Files, Working}, {cd, Dir}) ->
    Path = [Dir | Working],
    {maps:put(Path, 0, Files), Path};
acc({Files, Working}, ls) ->
    {Files, Working};
acc({Files, Working}, {dir, Dir}) ->
    {maps:put([Dir | Working], 0, Files), Working};
acc({Files, Working}, {file, Size, File}) ->
    {maps:put([File | Working], Size, Files), Working}.

-define(DISK, 70000000).
-define(NEED, 30000000).

fin({Files, _Working}) ->
    Sizes = du(Files),
    Free = ?DISK - maps:get(['/'], Sizes),
    F = fun(Dir, Size, Acc) when Size + Free >= ?NEED ->
                case Size < maps:get(Acc, Sizes) of
                    true ->
                        Dir;
                    false ->
                        Acc
                end;
           (_, _, Acc) ->
                Acc
        end,
    Dir = maps:fold(F, ['/'], Sizes),
    maps:get(Dir, Sizes).

du(Files) ->
    F = fun(_Dir, 0, Acc) ->
            Acc;
       ([_Filename | Path], Size, Acc) ->
            add_size(Path, Size, Acc)
    end,
    maps:fold(F, maps:new(), Files).

add_size([], _Size, Acc) ->
    Acc;
add_size(Path, Size, Acc) ->
    add_size(tl(Path), Size, maps:update_with(Path, fun(X) -> X + Size end, Size, Acc)).
