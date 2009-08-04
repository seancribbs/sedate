-module(sedate_step_collector).
-export([directory/1]).

directory(Dir) ->
  Bindings = create_bindings(),
  create_ets(),
  filelib:fold_files(Dir, ".*\\.erl", true, fun(File, _) -> ok = file:eval(File, Bindings) end, ok).

add_step(RE, Fun) ->
  ets:insert_new(sedate_steps, {RE, Fun}).

create_ets() ->
  ets:new(sedate_steps, [named_table, bag]).

create_bindings() ->
  Bindings = erl_eval:new_bindings(),
  lists:foldl(fun(Id, B) -> erl_eval:add_binding(Id, fun add_step/2, B) end, Bindings, ['Given', 'When', 'Then']).
