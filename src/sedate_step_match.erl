-module(sedate_step_match).
-export([find/1]).

find(String) ->
    find(String, ets:first(sedate_steps)).

find(String, '$end_of_table') -> not_found;
find(String, Key) ->
    [{StepRE, Fun}] = ets:lookup(sedate_steps, Key), 
    case re:run(StepRE, String, [{capture, all_but_first, list}]) of
        nomatch -> find(String, ets:next(sedate_steps, Key));     
        {match, Captures} -> {{StepRE, Fun}, Captures}
    end.
