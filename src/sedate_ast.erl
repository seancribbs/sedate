-module(sedate_ast).
-export([transform/3]).

%% Add clauses to this function to transform syntax nodes
%% from the parser into semantic output.
transform(feature, [_,_,_,Tags,_,{header, FeatureHeader},{bg,Background},Stories,_], _Index) ->
  {feature, Tags, lists:flatten(FeatureHeader), Background, Stories};
transform(tags, [_,{ts, Tags}],_Index) ->
  [Tag || [Tag, _] <- Tags];
transform(tag, [_, {tag_name, Name}],_Index) ->
  list_to_atom(lists:flatten(Name));
transform(background, [_,_,_,_,{name, Name},_,Steps],_Index) ->
  {background, lists:flatten(Name), Steps};
transform(scenario, [_, Tags, _, _, _, {name, Name}, _, Steps, _],_Index) ->
  {scenario, Name, Tags, Steps};
transform(scenario_outline, [_,Tags,_,_,_,{name, Name},_,Steps,Examples,_],_Index) ->
  {scenario_outline, Name, Tags, Steps, Examples};
transform(step, [_,_,Keyword,_,{name,Name},_,{multi,Multi},_],_Index) ->
  case Multi of
    [] ->
      {step, Keyword, string:strip(Name)};
    _ ->
      {step_multi, Keyword, string:strip(Name), Multi}
  end;
transform(py_string, [Open, {s, String}, _],_Index) ->
  Indentation = lists:flatten(proplists:get_value(indent, Open)),
  Unindented = re:replace(String, [$\n|Indentation], "\n", [global, {return, list}],_Index),
  {py_string, Unindented};
transform(examples, [_,_,_,{name,Name},_,Table,_],_Index) ->
  {examples, Name, Table};
transform(table, Node, _Index) ->
  {table, Node};
transform(table_row, [_,_,{cells, Cells},_,_],_Index) ->
  [string:strip(lists:flatten(Cell)) || [Cell, _] <- Cells];
transform(lines_to_keyword, Node, _Index) ->
  Lines = [lists:flatten(proplists:get_value(text, L)) || L <- Node],
  string:join(Lines, "\n");
transform(line_to_eol, Node, _Index) ->
  lists:flatten(Node);
transform(step_keyword, Node, _Index) ->
  list_to_atom(Node);
transform(Symbol, Node, _Index) when is_atom(Symbol) ->
  Node.
