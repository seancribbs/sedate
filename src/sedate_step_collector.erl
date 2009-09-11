-module(sedate_step_collector).
-export([directory/1]).

directory(Dir) ->
  Bindings = create_bindings(),
  create_ets(),
  filelib:fold_files(Dir, ".*\\.erl", true, fun(File, _) ->
                                                io:format("~p~n", [File]),
                                                ok = file:eval(File, Bindings) end, ok).

add_step(RE, Fun) ->
  ets:insert_new(sedate_steps, {RE, Fun}).

add_callback(When, Fun) ->
  io:format("~p~n", [When]),
  ets:insert(sedate_callbacks, {When, Fun}).

create_ets() ->
  ets:new(sedate_steps, [named_table, bag]),
  ets:new(sedate_callbacks, [named_table, bag]).

create_bindings() ->
  Bindings = erl_eval:new_bindings(),
  Bindings2 = lists:foldl(fun(Id, B) ->
                              erl_eval:add_binding(Id, fun add_step/2, B) end,
                          Bindings,
                          ['Given', 'When', 'Then']),
  lists:foldl(fun(Id, B) ->
                  erl_eval:add_binding(Id, fun(C) ->
                                               add_callback(Id, C) end, B) end,
              Bindings2,
              ['Before', 'After', 'AfterStep']).
