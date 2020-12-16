-module(aoc_2020_16_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do().

do() ->
    {Fields, Valid} = input_fields(),
    NF = maps:size(Fields),
    Your = input_your(),
    NCol = length(Your),
    Possibles = init_possibles(NCol, NF),
    Poss = input_nearby(Fields, Valid, Possibles),
    Rev = get_reverse_mapping(Poss),
    F = fun(I, Acc) ->
                Col = maps:get(I, Rev),
                Acc * lists:nth(Col, Your)
        end,
    lists:foldl(F, 1, lists:seq(1, 6)).

input_fields() ->
    input_fields_impl(maps:new(), sets:new(), 1).

input_fields_impl(Fields, Valid, I) ->
    case io:get_line("") of
        "\n" ->
            {Fields, Valid};
        L ->
            [_, Desc] = string:split(L, ":"),
            [_, Range1, _, Range2] = string:split(Desc, " ", all),
            {L1, R1, V1} = get_range(Range1, Valid),
            {L2, R2, V2} = get_range(Range2, V1),
            input_fields_impl(maps:put(I, {L1, R1, L2, R2}, Fields),
                              V2,
                              I + 1)
    end.

get_range(Str, Valid) ->
    [N1S, N2S] = string:split(Str, "-"),
    {N1, _} = string:to_integer(N1S),
    {N2, _} = string:to_integer(N2S),
    {N1, N2, add_to_valid_impl(Valid, N1, N2)}.

add_to_valid_impl(Valid, I, End) when I > End ->
    Valid;
add_to_valid_impl(Valid, I, End) ->
    add_to_valid_impl(sets:add_element(I, Valid), I + 1, End).

input_your() ->
    io:get_line(""),
    Res = input_your_impl(),
    io:get_line(""),
    Res.

input_your_impl() ->
    L = io:get_line(""),
    F = fun(NStr) ->
                {N, _} = string:to_integer(NStr),
                N
        end,
    lists:map(F, string:split(L, ",", all)).

init_possibles(NCol, NF) ->
    init_possibles_impl(NCol, NF, array:new(NCol + 1), 1).

init_possibles_impl(NCol, _NF, A, I) when I > NCol ->
    A;
init_possibles_impl(NCol, NF, A, I) ->
    init_possibles_impl(NCol, NF,
                        array:set(I, sets:from_list(lists:seq(1, NF)), A),
                        I + 1).



input_nearby(Fields, Valid, Possibles) ->
    io:get_line(""),
    input_nearby_impl(Fields, Valid, Possibles).

input_nearby_impl(Fields, Valid, Possibles) ->
    case io:get_line("") of
        eof ->
            Possibles;
        L ->
            NStrs = string:split(L, ",", all),
            F = fun(NStr, {IsValid, Acc}) ->
                        {N, _} = string:to_integer(NStr),
                        case sets:is_element(N, Valid) of
                            true ->
                                {IsValid, [N | Acc]};
                            _ ->
                                {false, [N | Acc]}
                        end
                end,
            {ValidB, Ns} = lists:foldl(F, {true, []}, NStrs),
            case ValidB of
                false ->
                    input_nearby_impl(Fields, Valid, Possibles);
                _ ->
                    Poss = update_possibles(lists:reverse(Ns),
                                            Fields,
                                            Possibles),
                    input_nearby_impl(Fields, Valid, Poss)
            end
    end.

update_possibles(Ns, Fields, Possibles) ->
    update_possibles_impl(Ns, Fields, Possibles, 1).

update_possibles_impl([], _, Possibles, _) ->
    Possibles;
update_possibles_impl([X | T], Fields, Possibles, I) ->
    Fs = array:get(I, Possibles),
    F = fun(Field, Acc) ->
                case possible(X, Field, Fields) of
                    true ->
                        Acc;
                    _ ->
                        sets:del_element(Field, Acc)
                end
        end,
    NewFs = sets:fold(F, Fs, Fs),
    update_possibles_impl(T, Fields, array:set(I, NewFs, Possibles), I + 1).

possible(X, Field, Fields) ->
    {L1, R1, L2, R2} = maps:get(Field, Fields),
    (L1 =< X andalso X =< R1) orelse (L2 =< X andalso X =< R2).

get_reverse_mapping(Possibles) ->
    get_reverse_mapping_impl(Possibles, maps:new()).

get_reverse_mapping_impl(Possibles, Rev) ->
    case {array:size(Possibles), maps:size(Rev)} of
        {N, M} when N =:= M + 1 ->
            Rev;
        _ ->
            {Col, Field} = find_unique(Possibles),
            get_reverse_mapping_impl(remove_field(Possibles, Field), maps:put(Field, Col, Rev))
    end.

find_unique(Possibles) ->
    find_unique_impl(Possibles, 1).

find_unique_impl(Possibles, Col) ->
    Fs = array:get(Col, Possibles),
    case sets:size(Fs) of
        1 ->
            [Field] = sets:to_list(Fs),
            {Col, Field};
        _ ->
            find_unique_impl(Possibles, Col + 1)
    end.

remove_field(Possibles, Field) ->
    remove_field_impl(Possibles, Field, 1, array:size(Possibles)).

remove_field_impl(Possibles, _Field, I, N) when I =:= N ->
    Possibles;
remove_field_impl(Possibles, Field, I, N) ->
    Possible = array:get(I, Possibles),
    case sets:is_element(Field, Possible) of
        true ->
            NewPossible = sets:del_element(Field, Possible),
            NewPossibles = array:set(I, NewPossible, Possibles),
            remove_field_impl(NewPossibles, Field, I + 1, N);
        _ ->
            remove_field_impl(Possibles, Field, I + 1, N)
    end.
