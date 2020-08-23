-module(aoc_2015_5_1).

-export([start/0]).

start() ->
    Out = do(),
    io:format("~p~n", [Out]),
    ok.

do() ->
    do_impl(0).

do_impl(X) ->
    case io:fread("", "~s") of
        {ok, [In]} ->
            case is_nice(In) of
                true ->
                    do_impl(X + 1);
                _ ->
                    do_impl(X)
            end;
        eof ->
            X
    end.

is_nice(X) ->
    is_nice_impl(X, 0, false).

is_nice_impl([], V, Repeated) ->
    (V >= 3) andalso Repeated;
is_nice_impl([C, C | T], V, false) ->
    is_nice_impl([C, C | T], V, true);
is_nice_impl([$a, $b | _], _, _) ->
    false;
is_nice_impl([$c, $d | _], _, _) ->
    false;
is_nice_impl([$p, $q | _], _, _) ->
    false;
is_nice_impl([$x, $y | _], _, _) ->
    false;
is_nice_impl([C | T], V, Repeated)
  when C =:= $a; C =:= $e; C =:= $i; C =:= $o; C =:= $u ->
    is_nice_impl(T, V + 1, Repeated);
is_nice_impl([_C | T], V, Repeated) ->
    is_nice_impl(T, V, Repeated).
