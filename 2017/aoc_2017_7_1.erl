-module(aoc_2017_7_1).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    run_impl(sets:new()).

run_impl(Acc) ->
    case input() of
        eof ->
            [Res] = sets:to_list(Acc),
            Res;
        X ->
            run_impl(acc(Acc, do(X)))
    end.

input() ->
    case io:get_line("") of
        eof ->
            eof;
        L ->
            Parts = string:split(L, " ", all),
            F = fun([H | _] = Word) when H >= $a, H =< $z ->
                        {true, string:trim(Word, trailing, "\n,")};
                   (_) ->
                        false
                end,
            lists:filtermap(F, Parts)
    end.

acc(Set, X) ->
    F = fun(Name, Acc) ->
                case sets:is_element(Name, Acc) of
                    true ->
                        sets:del_element(Name, Acc);
                    false ->
                        sets:add_element(Name, Acc)
                end
        end,
    lists:foldl(F, Set, X).

do(X) ->
    X.
