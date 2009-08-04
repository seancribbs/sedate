-module(test_suite).
-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, test_step_collector}].
