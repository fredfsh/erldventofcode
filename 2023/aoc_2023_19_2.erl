-module(aoc_2023_19_2).

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
                    eof;
                L ->
                    parse_workflow(L)
            end
    end.

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
    {Category, GLT, Threshold} = parse_condition(ConditionStr),
    {Category, GLT, Threshold, list_to_atom(WorkflowStr)}.

parse_condition(S) ->
    GLT = case lists:member($<, S) of
              true ->
                  $<;
              false ->
                  $>
          end,
    [CategoryStr, ThresholdStr] = string:split(S, [GLT]),
    {list_to_atom(CategoryStr), GLT, list_to_integer(ThresholdStr)}.

ini() ->
    maps:new().

do(X) ->
    X.

acc(Acc, #workflow{name = Name} = Workflow) ->
    maps:put(Name, Workflow, Acc).

-define(MIN, 0).
-define(MAX, 4001).
-define(CATEGORIES, [x, m, a, s]).

-define(IN, in).

fin(X) ->
    L = [{C, {?MIN, ?MAX}} || C <- ?CATEGORIES],
    Part = maps:from_list(L),
    dfs(?IN, Part, X).

-define(ACCEPTED, 'A').
-define(REJECTED, 'R').

dfs(?REJECTED, _, _) ->
    0;
dfs(?ACCEPTED, Part, _) ->
    combinations(Part);
dfs(Name, Part, Workflows) ->
    #workflow{rules = Rules, default = Default} = maps:get(Name, Workflows),
    dfs_impl(0, Part, Rules, Default, Workflows).

dfs_impl(Acc, Part, [], Default, Workflows) ->
    Acc + dfs(Default, Part, Workflows);
dfs_impl(Acc, Part, [{Category,$<,Threshold,Workflow}|T], Default, Workflows) ->
    case maps:get(Category, Part) of
        {_, Max} when Max =< Threshold ->
            Acc + dfs(Workflow, Part, Workflows);
        {Min, _} when Min >= Threshold - 1 ->
            dfs_impl(Acc, Part, T, Default, Workflows);
        {Min, Max} ->
            PartSatisfied = Part#{Category := {Min, Threshold}},
            dfs_impl(Acc + dfs(Workflow, PartSatisfied, Workflows),
                     Part#{Category := {Threshold - 1, Max}},
                     T, Default, Workflows)
    end;
dfs_impl(Acc, Part, [{Category,$>,Threshold,Workflow}|T], Default, Workflows) ->
    case maps:get(Category, Part) of
        {Min, _} when Min >= Threshold ->
            Acc + dfs(Workflow, Part, Workflows);
        {_, Max} when Max =< Threshold + 1 ->
            dfs_impl(Acc, Part, T, Default, Workflows);
        {Min, Max} ->
            PartSatisfied = Part#{Category := {Threshold, Max}},
            dfs_impl(Acc + dfs(Workflow, PartSatisfied, Workflows),
                     Part#{Category := {Min, Threshold + 1}},
                     T, Default, Workflows)
    end.

combinations(Part) ->
    F = fun(_, {Min, Max}, Acc) ->
                Acc * (Max - Min - 1)
        end,
    maps:fold(F, 1, Part).
