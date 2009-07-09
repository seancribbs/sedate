-module(sedate_parse).
-export([parse/1,file/1]).
-include_lib("neotoma/include/peg.hrl").

rule(feature) ->
  peg:seq([fun white/2, fun comment/2, fun white/2, fun tags/2, fun white/2, peg:label('header', peg:zero_or_more(peg:seq([peg:not_(peg:choose([fun scenario_outline/2, fun scenario/2, fun background/2])), peg:anything()]))), peg:label('bg', peg:optional(fun background/2)), fun feature_elements/2, peg:optional(fun comment/2)]);

rule(tags) ->
  peg:seq([fun white/2, peg:label('ts', peg:zero_or_more(peg:seq([fun tag/2, peg:one_or_more(peg:choose([fun space/2, fun eol/2]))])))]);

rule(tag) ->
  peg:seq([peg:string("@"), peg:label('tag_name', peg:one_or_more(peg:charclass("[^@\r\n\t\s]")))]);

rule(comment) ->
  peg:zero_or_more(peg:seq([fun comment_line/2, fun white/2]));

rule(comment_line) ->
  peg:seq([peg:string("#"), fun line_to_eol/2]);

rule(background) ->
  peg:seq([fun comment/2, fun white/2, fun background_keyword/2, peg:zero_or_more(fun space/2), peg:label('name', peg:optional(fun lines_to_keyword/2)), peg:choose([peg:one_or_more(fun eol/2), fun eof/2]), fun steps/2]);

rule(feature_elements) ->
  peg:zero_or_more(peg:choose([fun scenario/2, fun scenario_outline/2]));

rule(scenario) ->
  peg:seq([fun comment/2, fun tags/2, fun white/2, fun scenario_keyword/2, peg:zero_or_more(fun space/2), peg:label('name', fun lines_to_keyword/2), fun white/2, fun steps/2, fun white/2]);

rule(scenario_outline) ->
  peg:seq([fun comment/2, fun tags/2, fun white/2, fun scenario_outline_keyword/2, peg:zero_or_more(fun space/2), peg:label('name', fun lines_to_keyword/2), fun white/2, fun steps/2, fun examples_sections/2, fun white/2]);

rule(steps) ->
  peg:zero_or_more(fun step/2);

rule(step) ->
  peg:seq([fun comment/2, peg:zero_or_more(fun space/2), fun step_keyword/2, fun keyword_space/2, peg:label('name', fun line_to_eol/2), peg:choose([peg:one_or_more(fun eol/2), fun eof/2]), peg:label('multi', peg:optional(fun multiline_arg/2)), fun white/2]);

rule(examples_sections) ->
  peg:zero_or_more(fun examples/2);

rule(examples) ->
  peg:seq([peg:zero_or_more(fun space/2), fun examples_keyword/2, peg:zero_or_more(fun space/2), peg:label('name', peg:optional(fun lines_to_keyword/2)), fun eol/2, fun table/2, fun white/2]);

rule(multiline_arg) ->
  peg:choose([fun table/2, fun py_string/2]);

rule(line_to_eol) ->
  peg:zero_or_more(peg:seq([peg:not_(fun eol/2), peg:anything()]));

rule(line_to_keyword) ->
  peg:seq([fun white/2, peg:label('text', peg:one_or_more(peg:seq([peg:not_(fun step_keyword/2), peg:not_(fun scenario_keyword/2), peg:not_(fun scenario_outline_keyword/2), peg:not_(fun table/2), peg:not_(fun tag/2), peg:not_(fun comment_line/2), peg:not_(fun eol/2), peg:anything()])))]);

rule(lines_to_keyword) ->
  peg:zero_or_more(fun line_to_keyword/2);

rule(py_string) ->
  peg:seq([fun open_py_string/2, peg:label('s', peg:zero_or_more(peg:seq([peg:not_(fun close_py_string/2), peg:anything()]))), fun close_py_string/2]);

rule(open_py_string) ->
  peg:seq([peg:label('indent', peg:zero_or_more(fun space/2)), peg:string("\"\"\""), peg:zero_or_more(fun space/2), fun eol/2]);

rule(close_py_string) ->
  peg:seq([fun eol/2, peg:zero_or_more(fun space/2), peg:label('quotes', peg:string("\"\"\"")), fun white/2]);

rule(white) ->
  peg:zero_or_more(peg:choose([fun space/2, fun eol/2]));

rule(table) ->
  peg:one_or_more(fun table_row/2);

rule(table_row) ->
  peg:seq([peg:zero_or_more(fun space/2), peg:string("|"), peg:label('cells', peg:one_or_more(peg:seq([fun cell/2, peg:string("|")]))), peg:zero_or_more(fun space/2), peg:choose([peg:one_or_more(fun eol/2), fun eof/2])]);

rule(cell) ->
  peg:zero_or_more(peg:seq([peg:not_(peg:choose([peg:string("|"), fun eol/2])), peg:anything()]));

rule(space) ->
  peg:charclass("[ \t]");

rule(eol) ->
  peg:choose([peg:string("\n"), peg:seq([peg:string("\r"), peg:optional(peg:string("\n"))])]);

rule(eof) ->
  peg:not_(peg:anything());

rule(background_keyword) ->
  peg:string("Background:");

rule(scenario_keyword) ->
  peg:string("Scenario:");

rule(scenario_outline_keyword) ->
  peg:string("Scenario Outline:");

rule(step_keyword) ->
  peg:choose([peg:string("Given"), peg:string("When"), peg:string("Then"), peg:string("And"), peg:string("But")]);

rule(examples_keyword) ->
  peg:seq([peg:string("Examples"), peg:optional(peg:string(":"))]);

rule(keyword_space) ->
  peg:zero_or_more(fun space/2).

transform(Symbol,Node) -> sedate_ast:transform(Symbol, Node).