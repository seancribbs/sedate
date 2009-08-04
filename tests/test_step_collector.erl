-module(test_step_collector).
-include_lib("eunit/include/eunit.hrl").

directory_test_()->
  [
   fun single_file/0,
   fun multiple_files/0,
   fun deep_structure/0
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
  end.
