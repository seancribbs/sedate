-module(sedate_parse).
-export([parse/1,file/1]).
-compile(nowarn_unused_vars).
-compile({nowarn_unused_function,[p/4, p/5, p_eof/0, p_optional/1, p_not/1, p_assert/1, p_seq/1, p_and/1, p_choose/1, p_zero_or_more/1, p_one_or_more/1, p_label/2, p_string/1, p_anything/0, p_charclass/1, line/1, column/1]}).



-define(line, line(Idx)).
-define(item(I), lists:nth(I, Node)).
-define(label(L), proplists:get_value(L,Node,[])).

file(Filename) -> {ok, Bin} = file:read_file(Filename), parse(binary_to_list(Bin)).

parse(Input) ->
  setup_memo(),
  Result = case 'feature'(Input,{{line,1},{column,1}}) of
             {AST, [], _Index} -> AST;
             Any -> Any
           end,
  release_memo(), Result.

'feature'(Input, Index) ->
  p(Input, Index, 'feature', fun(I,D) -> (p_seq([fun 'white'/2, fun 'comment'/2, fun 'white'/2, fun 'tags'/2, fun 'white'/2, p_label('header', p_zero_or_more(p_seq([p_not(p_choose([fun 'scenario_outline'/2, fun 'scenario'/2, fun 'background'/2])), p_anything()]))), p_label('bg', p_optional(fun 'background'/2)), fun 'feature_elements'/2, p_optional(fun 'comment'/2)]))(I,D) end, fun(Node, Idx) -> 
{feature, ?item(4), lists:flatten(?label(header)), ?label(bg), ?item(8)}
 end).

'tags'(Input, Index) ->
  p(Input, Index, 'tags', fun(I,D) -> (p_seq([fun 'white'/2, p_label('ts', p_zero_or_more(p_seq([fun 'tag'/2, p_one_or_more(p_choose([fun 'space'/2, fun 'eol'/2]))])))]))(I,D) end, fun(Node, Idx) -> 
{tags, ?line, [Tag || [Tag, _] <- ?label(ts)]}
 end).

'tag'(Input, Index) ->
  p(Input, Index, 'tag', fun(I,D) -> (p_seq([p_string("@"), p_label('tag_name', p_one_or_more(p_charclass("[^@\r\n\t\s]")))]))(I,D) end, fun(Node, Idx) -> 
list_to_atom(lists:flatten(?label(tag_name)))
 end).

'comment'(Input, Index) ->
  p(Input, Index, 'comment', fun(I,D) -> (p_zero_or_more(p_seq([fun 'comment_line'/2, fun 'white'/2])))(I,D) end, fun(Node, Idx) -> Node end).

'comment_line'(Input, Index) ->
  p(Input, Index, 'comment_line', fun(I,D) -> (p_seq([p_string("#"), fun 'line_to_eol'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'background'(Input, Index) ->
  p(Input, Index, 'background', fun(I,D) -> (p_seq([fun 'comment'/2, fun 'white'/2, fun 'background_keyword'/2, p_zero_or_more(fun 'space'/2), p_label('name', p_optional(fun 'lines_to_keyword'/2)), p_choose([p_one_or_more(fun 'eol'/2), fun 'eof'/2]), fun 'steps'/2]))(I,D) end, fun(Node, Idx) -> 
{background, ?line, lists:flatten(?label(name)), ?item(7)}
 end).

'feature_elements'(Input, Index) ->
  p(Input, Index, 'feature_elements', fun(I,D) -> (p_zero_or_more(p_choose([fun 'scenario'/2, fun 'scenario_outline'/2])))(I,D) end, fun(Node, Idx) -> Node end).

'scenario'(Input, Index) ->
  p(Input, Index, 'scenario', fun(I,D) -> (p_seq([fun 'comment'/2, fun 'tags'/2, fun 'white'/2, fun 'scenario_keyword'/2, p_zero_or_more(fun 'space'/2), p_label('name', fun 'lines_to_keyword'/2), fun 'white'/2, fun 'steps'/2, fun 'white'/2]))(I,D) end, fun(Node, Idx) -> 
{scenario, ?line, ?label(name), ?item(2), ?item(8)}
 end).

'scenario_outline'(Input, Index) ->
  p(Input, Index, 'scenario_outline', fun(I,D) -> (p_seq([fun 'comment'/2, fun 'tags'/2, fun 'white'/2, fun 'scenario_outline_keyword'/2, p_zero_or_more(fun 'space'/2), p_label('name', fun 'lines_to_keyword'/2), fun 'white'/2, fun 'steps'/2, fun 'examples_sections'/2, fun 'white'/2]))(I,D) end, fun(Node, Idx) -> 
{scenario_outline, ?line, ?label(name), ?item(2), ?item(8), ?item(9)}
 end).

'steps'(Input, Index) ->
  p(Input, Index, 'steps', fun(I,D) -> (p_zero_or_more(fun 'step'/2))(I,D) end, fun(Node, Idx) -> Node end).

'step'(Input, Index) ->
  p(Input, Index, 'step', fun(I,D) -> (p_seq([fun 'comment'/2, p_zero_or_more(fun 'space'/2), fun 'step_keyword'/2, fun 'keyword_space'/2, p_label('name', fun 'line_to_eol'/2), p_choose([p_one_or_more(fun 'eol'/2), fun 'eof'/2]), p_label('multi', p_optional(fun 'multiline_arg'/2)), fun 'white'/2]))(I,D) end, fun(Node, Idx) -> 
case ?label(multi) of
  [] -> {step, ?line, ?item(3), string:strip(?label(name))};
  _  -> {step_multi, ?line, ?item(3), string:strip(?label(name)), ?label(multi)}
end
 end).

'examples_sections'(Input, Index) ->
  p(Input, Index, 'examples_sections', fun(I,D) -> (p_zero_or_more(fun 'examples'/2))(I,D) end, fun(Node, Idx) -> Node end).

'examples'(Input, Index) ->
  p(Input, Index, 'examples', fun(I,D) -> (p_seq([p_zero_or_more(fun 'space'/2), fun 'examples_keyword'/2, p_zero_or_more(fun 'space'/2), p_label('name', p_optional(fun 'lines_to_keyword'/2)), fun 'eol'/2, fun 'table'/2, fun 'white'/2]))(I,D) end, fun(Node, Idx) -> 
{examples, ?line, ?label(name), ?item(6)}
 end).

'multiline_arg'(Input, Index) ->
  p(Input, Index, 'multiline_arg', fun(I,D) -> (p_choose([fun 'table'/2, fun 'py_string'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'line_to_eol'(Input, Index) ->
  p(Input, Index, 'line_to_eol', fun(I,D) -> (p_zero_or_more(p_seq([p_not(fun 'eol'/2), p_anything()])))(I,D) end, fun(Node, Idx) -> lists:flatten(Node) end).

'line_to_keyword'(Input, Index) ->
  p(Input, Index, 'line_to_keyword', fun(I,D) -> (p_seq([fun 'white'/2, p_label('text', p_one_or_more(p_seq([p_not(fun 'step_keyword'/2), p_not(fun 'scenario_keyword'/2), p_not(fun 'scenario_outline_keyword'/2), p_not(fun 'table'/2), p_not(fun 'tag'/2), p_not(fun 'comment_line'/2), p_not(fun 'eol'/2), p_anything()])))]))(I,D) end, fun(Node, Idx) -> Node end).

'lines_to_keyword'(Input, Index) ->
  p(Input, Index, 'lines_to_keyword', fun(I,D) -> (p_zero_or_more(fun 'line_to_keyword'/2))(I,D) end, fun(Node, Idx) -> 
Lines = [lists:flatten(proplists:get_value(text, L)) || L <- Node],
string:join(Lines, "\n")
 end).

'py_string'(Input, Index) ->
  p(Input, Index, 'py_string', fun(I,D) -> (p_seq([fun 'open_py_string'/2, p_label('s', p_zero_or_more(p_seq([p_not(fun 'close_py_string'/2), p_anything()]))), fun 'close_py_string'/2]))(I,D) end, fun(Node, Idx) -> 
Indentation = lists:flatten(proplists:get_value(indent, ?item(1))),
Unindented = re:replace(?label(s), [$\n|Indentation], "\n", [global, {return, list}]),
{py_string, ?line, Unindented}
 end).

'open_py_string'(Input, Index) ->
  p(Input, Index, 'open_py_string', fun(I,D) -> (p_seq([p_label('indent', p_zero_or_more(fun 'space'/2)), p_string("\"\"\""), p_zero_or_more(fun 'space'/2), fun 'eol'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'close_py_string'(Input, Index) ->
  p(Input, Index, 'close_py_string', fun(I,D) -> (p_seq([fun 'eol'/2, p_zero_or_more(fun 'space'/2), p_label('quotes', p_string("\"\"\"")), fun 'white'/2]))(I,D) end, fun(Node, Idx) -> Node end).

'white'(Input, Index) ->
  p(Input, Index, 'white', fun(I,D) -> (p_zero_or_more(p_choose([fun 'space'/2, fun 'eol'/2])))(I,D) end, fun(Node, Idx) -> Node end).

'table'(Input, Index) ->
  p(Input, Index, 'table', fun(I,D) -> (p_one_or_more(fun 'table_row'/2))(I,D) end, fun(Node, Idx) -> 
{table, ?line, Node}
 end).

'table_row'(Input, Index) ->
  p(Input, Index, 'table_row', fun(I,D) -> (p_seq([p_zero_or_more(fun 'space'/2), p_string("|"), p_label('cells', p_one_or_more(p_seq([fun 'cell'/2, p_string("|")]))), p_zero_or_more(fun 'space'/2), p_choose([p_one_or_more(fun 'eol'/2), fun 'eof'/2])]))(I,D) end, fun(Node, Idx) -> 
{row, ?line, [string:strip(lists:flatten(Cell)) || [Cell, _] <- ?label(cells)]}
 end).

'cell'(Input, Index) ->
  p(Input, Index, 'cell', fun(I,D) -> (p_zero_or_more(p_seq([p_not(p_choose([p_string("|"), fun 'eol'/2])), p_anything()])))(I,D) end, fun(Node, Idx) -> Node end).

'space'(Input, Index) ->
  p(Input, Index, 'space', fun(I,D) -> (p_charclass("[ \t]"))(I,D) end, fun(Node, Idx) -> Node end).

'eol'(Input, Index) ->
  p(Input, Index, 'eol', fun(I,D) -> (p_choose([p_string("\n"), p_seq([p_string("\r"), p_optional(p_string("\n"))])]))(I,D) end, fun(Node, Idx) -> Node end).

'eof'(Input, Index) ->
  p(Input, Index, 'eof', fun(I,D) -> (p_not(p_anything()))(I,D) end, fun(Node, Idx) -> Node end).

'background_keyword'(Input, Index) ->
  p(Input, Index, 'background_keyword', fun(I,D) -> (p_string("Background:"))(I,D) end, fun(Node, Idx) -> Node end).

'scenario_keyword'(Input, Index) ->
  p(Input, Index, 'scenario_keyword', fun(I,D) -> (p_string("Scenario:"))(I,D) end, fun(Node, Idx) -> Node end).

'scenario_outline_keyword'(Input, Index) ->
  p(Input, Index, 'scenario_outline_keyword', fun(I,D) -> (p_string("Scenario Outline:"))(I,D) end, fun(Node, Idx) -> Node end).

'step_keyword'(Input, Index) ->
  p(Input, Index, 'step_keyword', fun(I,D) -> (p_choose([p_string("Given"), p_string("When"), p_string("Then"), p_string("And"), p_string("But")]))(I,D) end, fun(Node, Idx) -> list_to_atom(Node) end).

'examples_keyword'(Input, Index) ->
  p(Input, Index, 'examples_keyword', fun(I,D) -> (p_seq([p_string("Examples"), p_optional(p_string(":"))]))(I,D) end, fun(Node, Idx) -> Node end).

'keyword_space'(Input, Index) ->
  p(Input, Index, 'keyword_space', fun(I,D) -> (p_zero_or_more(fun 'space'/2))(I,D) end, fun(Node, Idx) -> Node end).






p(Inp, Index, Name, ParseFun) ->
  p(Inp, Index, Name, ParseFun, fun(N, _Idx) -> N end).

p(Inp, StartIndex, Name, ParseFun, TransformFun) ->
  % Grab the memo table from ets
  Memo = get_memo(StartIndex),
  % See if the current reduction is memoized
  case dict:find(Name, Memo) of
    % If it is, return the result
    {ok, Result} -> Result;
    % If not, attempt to parse
    _ ->
      case ParseFun(Inp, StartIndex) of
        % If it fails, memoize the failure
        {fail,_} = Failure ->
          memoize(StartIndex, dict:store(Name, Failure, Memo)),
          Failure;
        % If it passes, transform and memoize the result.
        {Result, InpRem, NewIndex} ->
          Transformed = TransformFun(Result, StartIndex),
          memoize(StartIndex, dict:store(Name, {Transformed, InpRem, NewIndex}, Memo)),
          {Transformed, InpRem, NewIndex}
      end
  end.

setup_memo() ->
  put(parse_memo_table, ets:new(?MODULE, [set])).

release_memo() ->
  ets:delete(memo_table_name()).

memoize(Position, Struct) ->
  ets:insert(memo_table_name(), {Position, Struct}).

get_memo(Position) ->
  case ets:lookup(memo_table_name(), Position) of
    [] -> dict:new();
    [{Position, Dict}] -> Dict
  end.

memo_table_name() ->
    get(parse_memo_table).

p_eof() ->
  fun([], Index) -> {eof, [], Index};
     (_, Index) -> {fail, {expected, eof, Index}} end.

p_optional(P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} -> {[], Input, Index};
        {_, _, _} = Success -> Success
      end
  end.

p_not(P) ->
  fun(Input, Index)->
      case P(Input,Index) of
        {fail,_} ->
          {[], Input, Index};
        {Result, _, _} -> {fail, {expected, {no_match, Result},Index}}
      end
  end.

p_assert(P) ->
  fun(Input,Index) ->
      case P(Input,Index) of
        {fail,_} = Failure-> Failure;
        _ -> {[], Input, Index}
      end
  end.

p_and(P) ->
  p_seq(P).

p_seq(P) ->
  fun(Input, Index) ->
      p_all(P, Input, Index, [])
  end.

p_all([], Inp, Index, Accum ) -> {lists:reverse( Accum ), Inp, Index};
p_all([P|Parsers], Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail, _} = Failure -> Failure;
    {Result, InpRem, NewIndex} -> p_all(Parsers, InpRem, NewIndex, [Result|Accum])
  end.

p_choose(Parsers) ->
  fun(Input, Index) ->
      p_attempt(Parsers, Input, Index, none)
  end.

p_attempt([], _Input, _Index, Failure) -> Failure;
p_attempt([P|Parsers], Input, Index, FirstFailure)->
  case P(Input, Index) of
    {fail, _} = Failure ->
      case FirstFailure of
        none -> p_attempt(Parsers, Input, Index, Failure);
        _ -> p_attempt(Parsers, Input, Index, FirstFailure)
      end;
    Result -> Result
  end.

p_zero_or_more(P) ->
  fun(Input, Index) ->
      p_scan(P, Input, Index, [])
  end.

p_one_or_more(P) ->
  fun(Input, Index)->
      Result = p_scan(P, Input, Index, []),
      case Result of
        {[_|_], _, _} ->
          Result;
        _ ->
          {fail, {expected, Failure, _}} = P(Input,Index),
          {fail, {expected, {at_least_one, Failure}, Index}}
      end
  end.

p_label(Tag, P) ->
  fun(Input, Index) ->
      case P(Input, Index) of
        {fail,_} = Failure ->
           Failure;
        {Result, InpRem, NewIndex} ->
          {{Tag, Result}, InpRem, NewIndex}
      end
  end.

p_scan(_, [], Index, Accum) -> {lists:reverse( Accum ), [], Index};
p_scan(P, Inp, Index, Accum) ->
  case P(Inp, Index) of
    {fail,_} -> {lists:reverse(Accum), Inp, Index};
    {Result, InpRem, NewIndex} -> p_scan(P, InpRem, NewIndex, [Result | Accum])
  end.

p_string(S) ->
  fun(Input, Index) ->
      case lists:prefix(S, Input) of
        true -> {S, lists:sublist(Input, length(S)+1, length(Input)), p_advance_index(S,Index)};
        _ -> {fail, {expected, {string, S}, Index}}
      end
  end.

p_anything() ->
  fun([], Index) -> {fail, {expected, any_character, Index}};
     ([H|T], Index) -> {H, T, p_advance_index(H, Index)}
  end.

p_charclass(Class) ->
  fun(Inp, Index) ->
     {ok, RE} = re:compile("^"++Class),
      case re:run(Inp, RE) of
        {match, _} ->
          {hd(Inp), tl(Inp), p_advance_index(hd(Inp), Index)};
        _ -> {fail,{expected, {character_class, Class}, Index}}
      end
  end.

line({{line,L},_}) -> L;
line(_) -> undefined.

column({_,{column,C}}) -> C;
column(_) -> undefined.

p_advance_index(MatchedInput, Index) when is_list(MatchedInput) -> % strings
  lists:foldl(fun p_advance_index/2, Index, MatchedInput);
p_advance_index(MatchedInput, Index) when is_integer(MatchedInput) -> % single characters
  {{line, Line}, {column, Col}} = Index,
  case MatchedInput of
    $\n -> {{line, Line+1}, {column, 1}};
    _ -> {{line, Line}, {column, Col+1}}
  end.
