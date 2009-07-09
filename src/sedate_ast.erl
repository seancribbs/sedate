-module(sedate_ast).
-export([transform/2]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(feature, [_,_,_,Tags,_,{header, FeatureHeader},{bg,Background},Stories,_]) ->
  {feature, Tags, lists:flatten(FeatureHeader), Background, Stories};
transform(tags, [_,{ts, Tags}]) ->
  [Tag || [Tag, _] <- Tags];
transform(tag, [_, {tag_name, Name}]) ->
  list_to_atom(lists:flatten(Name));
transform(background, [_,_,_,_,{name, Name},_,Steps]) ->
  {background, lists:flatten(Name), Steps};
transform(scenario, [_, Tags, _, _, _, {name, Name}, _, Steps, _]) ->
  {scenario, Name, Tags, Steps};
transform(scenario_outline, [_,Tags,_,_,_,{name, Name},_,Steps,Examples,_]) ->
  {scenario_outline, Name, Tags, Steps, Examples};
transform(step, [_,_,Keyword,_,{name,Name},_,{multi,Multi},_]) ->
  case Multi of
    [] ->
      {step, Keyword, Name};
    _ ->
      {step_multi, Keyword, Name, Multi}
  end;
transform(py_string, [Open, {s, String}, _]) ->
  Indentation = lists:flatten(proplists:get_value(indent, Open)),
  Unindented = re:replace(String, [$\n|Indentation], "\n", [global, {return, list}]),
  {py_string, Unindented};
transform(examples, [_,_,_,{name,Name},_,Table,_]) ->
  {examples, Name, Table};
transform(table, Node) ->
  {table, Node};
transform(table_row, [_,_,{cells, Cells},_,_]) ->
  [string:strip(lists:flatten(Cell)) || [Cell, _] <- Cells];
transform(lines_to_keyword, Node) ->
  Lines = [lists:flatten(proplists:get_value(text, L)) || L <- Node],
  string:join(Lines, "\n");
transform(line_to_eol, Node) ->
  lists:flatten(Node);
transform(step_keyword, Node) ->
  list_to_atom(Node);
transform(Symbol, Node) when is_atom(Symbol) ->
  Node.
