-module(aoc_2023_19_1).

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
    case io:get_line("") of
        eof ->
            eof;
        Line ->
            case string:trim(Line, trailing, "}\n") of
                [] ->
                    ok;
                [${ | T] ->
                    parse_part(T);
                L ->
                    parse_workflow(L)
            end
    end.

parse_part(S) ->
    Parts = string:split(S, ",", all),
    L = [begin
             [CategoryStr, RatingStr] = string:split(P, "="),
             {list_to_atom(CategoryStr), list_to_integer(RatingStr)}
         end || P <- Parts],
    maps:from_list(L).

-record(workflow, {name, rules, default}).

parse_workflow(S) ->
    [NameStr, RulesStr] = string:split(S, "{"),
    [StrictRulesStr, DefaultStr] = string:split(RulesStr, ",", trailing),
    Name = list_to_atom(NameStr),
    Rules = parse_rules(StrictRulesStr),
    Default = list_to_atom(DefaultStr),
    #workflow{name = Name, rules = Rules, default = Default}.

parse_rules(S) ->
    Parts = string:split(S, ",", all),
    [parse_rule(P) || P <- Parts].

parse_rule(S) ->
    [ConditionStr, WorkflowStr] = string:split(S, ":"),
    {Category, Fn} = parse_condition(ConditionStr),
    {Category, Fn, list_to_atom(WorkflowStr)}.

parse_condition(S) ->
    case string:split(S, ">") of
        [CategoryStr, ThresholdStr] ->
            N = list_to_integer(ThresholdStr),
            {list_to_atom(CategoryStr), fun(X) -> X > N end};
        _ ->
            [CategoryStr, ThresholdStr] = string:split(S, "<"),
            N = list_to_integer(ThresholdStr),
            {list_to_atom(CategoryStr), fun(X) -> X < N end}
    end.

ini() ->
    {0, maps:new()}.

do(X) ->
    X.

-define(ACCEPTED, 'A').
-define(REJECTED, 'R').

acc(Acc, ok) ->
    Acc;
acc({Acc, Workflows}, #workflow{name = Name} = Workflow) ->
    {Acc, maps:put(Name, Workflow, Workflows)};
acc({Acc, Workflows}, #{} = Part) ->
    case flow(Part, Workflows) of
        ?ACCEPTED ->
            {Acc + rating(Part), Workflows};
        ?REJECTED ->
            {Acc, Workflows}
    end.

-define(IN, in).

flow(Part, Workflows) ->
    flow_impl(?IN, Part, Workflows).

-define(UNMATCHED, 'U').

flow_impl(?ACCEPTED, _, _) ->
    ?ACCEPTED;
flow_impl(?REJECTED, _, _) ->
    ?REJECTED;
flow_impl(Name, Part, Workflows) ->
    #workflow{rules = Rules, default = Default} = maps:get(Name, Workflows),
    Next = case match(Rules, Part) of
               ?UNMATCHED ->
                   Default;
               Matched ->
                   Matched
           end,
    flow_impl(Next, Part, Workflows).

match([], _) ->
    ?UNMATCHED;
match([{Category, Fn, Workflow} | T], Part) ->
    case Fn(maps:get(Category, Part)) of
        true ->
            Workflow;
        false ->
            match(T, Part)
    end.

rating(Part) ->
    lists:sum(maps:values(Part)).

fin({Acc, _Workflows}) ->
    Acc.
