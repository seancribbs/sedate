-module(test_step_match).
-include_lib("eunit/include/eunit.hrl").

step_match_test() ->
    test_step_collector:cleanup_table(),
    sedate_step_collector:directory("tests/fixtures/collector"),
    ?assertMatch({{"given", _}, L} when is_list(L), sedate_step_match:find("given")).
