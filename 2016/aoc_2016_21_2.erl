-module(aoc_2016_21_2).

-export([start/0]).

start() ->
    Out = run(),
    io:format("~p~n", [Out]),
    ok.

run() ->
    do(input()).

input() ->
    input_impl([]).

input_impl(Acc) ->
    case io:fread("", "~a ~a") of
        eof ->
            Acc;
        {ok, [swap, position]} ->
            {ok, [A, B]} = io:fread("", "~d with position ~d"),
            input_impl([{swap, A, B} | Acc]);
        {ok, [swap, letter]} ->
            {ok, [A, B]} = io:fread("", " ~c with letter ~c"),
            input_impl([{swap, A, B} | Acc]);
        {ok, [rotate, based]} ->
            {ok, [A]} = io:fread("", " on position of letter ~c"),
            input_impl([{rotate, A} | Acc]);
        {ok, [rotate, Dir]} ->
            {ok, [A, _]} = io:fread("", "~d ~s"),
            input_impl([{rotate, Dir, A} | Acc]);
        {ok, [reverse, positions]} ->
            {ok, [A, B]} = io:fread("", "~d through ~d"),
            input_impl([{reverse, A, B} | Acc]);
        {ok, [move, position]} ->
            {ok, [A, B]} = io:fread("", "~d to position ~d"),
            input_impl([{move, A, B} | Acc])
    end.

-define(INIT, "fbgdceah").

do(X) ->
    do_impl(?INIT, X).

do_impl(S, []) ->
    S;
do_impl(S, [H | T]) ->
    do_impl(unscramble(S, H), T).

unscramble(S, {swap, [A], [B]}) ->
    swap(S, A, B);
unscramble(S, {swap, A, B}) ->
    swap(S, lists:nth(A + 1, S), lists:nth(B + 1, S));
unscramble(S, {rotate, left, A}) ->
    rotate_right(S, A);
unscramble(S, {rotate, right, A}) ->
    rotate_left(S, A);
unscramble(S, {rotate, [A]}) ->
    inverse_rotate_index(S, A);
unscramble(S, {reverse, A, B}) ->
    reverse(S, A, B);
unscramble(S, {move, A, B}) ->
    move(S, B, A).

swap(S, A, B) ->
    swap_impl([], S, A, B).

swap_impl(Acc, [], _, _) ->
    lists:reverse(Acc);
swap_impl(Acc, [A | T], A, B) ->
    swap_impl([B | Acc], T, A, B);
swap_impl(Acc, [B | T], A, B) ->
    swap_impl([A | Acc], T, A, B);
swap_impl(Acc, [H | T], A, B) ->
    swap_impl([H | Acc], T, A, B).

rotate_right(S, A) ->
    rotate_left(S, length(S) - A rem length(S)).

rotate_left(S, A) ->
    rotate_left_impl(S, A rem length(S)).

rotate_left_impl(S, A) ->
    lists:append(string:slice(S, A), string:slice(S, 0, A)).

inverse_rotate_index(S, A) ->
    inverse_rotate_index_impl(S, S, A).

inverse_rotate_index_impl(Inverse, S, A) ->
    case rotate_index(Inverse, A) of
        S ->
            Inverse;
        _ ->
            inverse_rotate_index_impl(rotate_right(Inverse, 1), S, A)
    end.

rotate_index(S, A) ->
    Rotates = case index(A, S) of
                  X when X >= 4 ->
                      X + 2;
                  Y ->
                      Y + 1
              end,
    rotate_right(S, Rotates).

index(A, S) ->
    index_impl(0, A, S).

index_impl(I, A, [A | _]) ->
    I;
index_impl(I, A, [_ | T]) ->
    index_impl(I + 1, A, T).

reverse(S, A, B) ->
    reverse_impl([], 0, [], S, A, B).

reverse_impl(Acc, _, _, [], _, _) ->
    lists:reverse(Acc);
reverse_impl(Acc, I, [], [H | T], A, B) when I < A; I > B ->
    reverse_impl([H | Acc], I + 1, [], T, A, B);
reverse_impl(Acc, B, RevAcc, [H | T], A, B) ->
    NewAcc = lists:append(lists:reverse([H | RevAcc]), Acc),
    reverse_impl(NewAcc, B + 1, [], T, A, B);
reverse_impl(Acc, I, RevAcc, [H | T], A, B) ->
    reverse_impl(Acc, I + 1, [H | RevAcc], T, A, B).

move(S, A, B) ->
    {C, S2} = remove(S, A),
    insert(C, B, S2).

remove(S, A) ->
    remove_impl([], 0, S, A).

remove_impl(Acc, A, [H | T], A) ->
    {H, lists:append(lists:reverse(Acc), T)};
remove_impl(Acc, I, [H | T], A) ->
    remove_impl([H | Acc], I + 1, T, A).

insert(C, I, S) ->
    lists:append([string:slice(S, 0, I), [C], string:slice(S, I)]).
