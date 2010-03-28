-module(sedate_step_collector).
-export([directory/1]).

%% TODO - capture the line of each step definition

directory(Dir) ->
  Bindings = create_bindings(),
  create_ets(),
  filelib:fold_files(Dir, ".*\\.erl", true, fun(File, _) ->
                                                ok = file:eval(File, Bindings) end, ok).

add_step(RE, Fun) ->
  ets:insert_new(sedate_steps, {RE, Fun}).

add_callback(When, Tags, Fun) ->
  ets:insert(sedate_callbacks, {{When, Tags}, Fun}).

create_ets() ->
  ets:new(sedate_steps, [named_table, bag]),
  ets:new(sedate_callbacks, [named_table, bag]).

create_bindings() ->
  Bindings = erl_eval:new_bindings(),
  Bindings2 = lists:foldl(fun(Id, B) ->
                              erl_eval:add_binding(Id, fun add_step/2, B) end,
                          Bindings,
                          ['Given', 'When', 'Then']),
  Bindings3 = lists:foldl(fun(Id, B) ->
                  erl_eval:add_binding(Id, fun(Tags, C) ->
                                               add_callback(Id, Tags, C) end, B) end,
              Bindings2,
              ['Before', 'After', 'AfterStep']),
  erl_eval:add_binding('AfterConfiguration', fun(C) ->
                                                  add_callback('AfterConfiguration', [], C) end, Bindings3).
