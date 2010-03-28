-module(test_step_collector).
-include_lib("eunit/include/eunit.hrl").
-export([cleanup_table/0]).

directory_test_()->
  [
   fun single_file/0,
   fun multiple_files/0,
   fun deep_structure/0,
   fun callbacks/0
  ].

single_file() ->
  cleanup_table(),
  sedate_step_collector:directory("tests/fixtures/collector/single"),
  ?assertMatch([{"single", X}] when is_function(X), ets:lookup(sedate_steps, "single")).

multiple_files() ->
  cleanup_table(),
  sedate_step_collector:directory("tests/fixtures/collector/multiple"),
  ?assertMatch([{"given", X}] when is_function(X), ets:lookup(sedate_steps, "given")),
  ?assertMatch([{"when", Y}] when is_function(Y), ets:lookup(sedate_steps, "when")).

deep_structure() ->
  cleanup_table(),
  sedate_step_collector:directory("tests/fixtures/collector/deep"),
  ?assertMatch([{"given", X}] when is_function(X), ets:lookup(sedate_steps, "given")),
  ?assertMatch([{"when", Y}] when is_function(Y), ets:lookup(sedate_steps, "when")),
  ?assertMatch([{"deep", Z}] when is_function(Z), ets:lookup(sedate_steps, "deep")).

cleanup_table() ->
  case ets:info(sedate_steps) of
    undefined ->
      ok;
    _ ->
      ets:delete(sedate_steps)
  end,
  case ets:info(sedate_callbacks) of
    undefined ->
      ok;
    _ ->
      ets:delete(sedate_callbacks)
  end.

callbacks() ->
  cleanup_table(),
  sedate_step_collector:directory("tests/fixtures/collector/callbacks"),
  ?assertMatch([{{'Before', []}, X}] when is_function(X), ets:lookup(sedate_callbacks, {'Before',[]})),
  ?assertMatch([{{'After',[]}, Y}] when is_function(Y), ets:lookup(sedate_callbacks, {'After',[]})),
  ?assertMatch([{{'AfterStep',[]}, Z}] when is_function(Z), ets:lookup(sedate_callbacks, {'AfterStep',[]})),
  ?assertMatch([{{'Before', ["@wip"]}, Q}] when is_function(Q), ets:lookup(sedate_callbacks, {'Before', ["@wip"]})),
    ?assertMatch([{{'AfterConfiguration',[]}, R}] when is_function(R), ets:lookup(sedate_callbacks, {'AfterConfiguration',[]})).
