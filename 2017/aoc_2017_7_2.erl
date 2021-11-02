-module(aoc_2017_7_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl(maps:new()).

-record(disc, {self, total, subs = []}).

input_impl(Acc) ->
    case io:get_line("") of
        eof ->
            Acc;
        L ->
            {Name, Disc} = case string:split(L, "->") of
                               [P] ->
                                   {Base, Weight} = input_base(P),
                                   {Base, #disc{self = Weight}};
                               [P1, P2] ->
                                   {Base, Weight} = input_base(P1),
                                   Subs = input_subs(P2),
                                   {Base, #disc{self = Weight, subs = Subs}}
                           end,
            input_impl(maps:put(Name, Disc, Acc))
    end.

input_base(S) ->
    [Name, WS] = string:split(string:trim(S, trailing), " "),
    {list_to_atom(Name), list_to_integer(string:slice(WS, 1, length(WS) - 2))}.

input_subs(S) ->
    Trimmed = string:split(string:trim(S), " ", all),
    [list_to_atom(string:trim(X, trailing, ",")) || X <- Trimmed].

do(X) ->
    Root = root(X),
    Discs = fill_total(Root, X),
    reba(Root, Discs).

root(Discs) ->
    G = fun(Name, Set) ->
                case sets:is_element(Name, Set) of
                    true  -> sets:del_element(Name, Set);
                    false -> sets:add_element(Name, Set)
                end
        end,
    F = fun(Base, #disc{subs = Subs}, Acc) ->
                lists:foldl(G, Acc, [Base | Subs])
        end,
    [Root] = sets:to_list(maps:fold(F, sets:new(), Discs)),
    Root.

fill_total(Root, Discs) ->
    Disc = maps:get(Root, Discs),
    #disc{self = Self, subs = Subs} = Disc,
    F = fun(Sub, {DiscsAcc, TotalAcc}) ->
                NewDiscsAcc = fill_total(Sub, DiscsAcc),
                SubDisc = maps:get(Sub, NewDiscsAcc),
                {NewDiscsAcc, TotalAcc + SubDisc#disc.total}
        end,
    {SubsFilled, SubsTotal} = lists:foldl(F, {Discs, 0}, Subs),
    maps:put(Root, Disc#disc{total = Self + SubsTotal}, SubsFilled).

reba(Root, Discs) ->
    case maps:get(Root, Discs) of
        #disc{subs = [Sub]} ->
            reba(Sub, Discs);
        #disc{subs = [Sub1, Sub2]} ->
            case reba(Sub1, Discs) of
                undefined ->
                    reba(Sub2, Discs);
                N ->
                    N
            end;
        #disc{subs = Subs} ->
            F = fun(Sub) ->
                        #disc{total = Total} = maps:get(Sub, Discs),
                        {Total, Sub}
                end,
            L = lists:map(F, Subs),
            {Target, Wrong} =
                case lists:keysort(1, L) of
                    [{Same, _}, {Same, _} | _] = Sorted ->
                        case lists:last(Sorted) of
                            {Same, _} ->
                                undefined;
                            {_, What} ->
                                {Same, What}
                        end;
                    [{_, What}, {Same, _} | _] ->
                        {Same, What}
                end,
            reba(Wrong, Target, Discs)
    end.

reba(Root, Target, Discs) ->
    case maps:get(Root, Discs) of
        #disc{subs = []} ->
            Target;
        #disc{self = Self, subs = [Sub]} ->
            reba(Sub, Target - Self, Discs);
        #disc{self = Self, subs = Subs} ->
            N = length(Subs),
            case same(Subs, Discs) of
                false ->
                    SubTarget = (Target - Self) div N,
                    reba_subs(Subs, SubTarget, Discs);
                Same ->
                    Target - Same * N
            end
    end.

same([H | T], Discs) ->
    #disc{total = First} = maps:get(H, Discs),
    F = fun(Name) ->
                #disc{total = Total} = maps:get(Name, Discs),
                Total =:= First
        end,
    case lists:dropwhile(F, T) of
        [] ->
            First;
        _ ->
            false
    end.

reba_subs([H | T], Target, Discs) ->
    case maps:get(H, Discs) of
        #disc{total = Target} ->
            reba_subs(T, Target, Discs);
        _ ->
            reba(H, Target, Discs)
    end.
