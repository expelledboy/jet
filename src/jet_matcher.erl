-module(jet_matcher).
-export([ compile/1 ]).
-export([ run/2 ]).

% MatchList
%[{"case":"has_name","type":"object","properties":{"name":true}}
% {"case":"is_flag","type":"boolean"}]

compile(MatchList) when is_list(MatchList) ->
  CompilePatterns = MatchList,
  % CompilePattern list of matcher functions
  {ok, CompilePatterns}.

run(CompilePatterns, _Json) ->
  SearchPattern = fun(_CasePattern) ->
                      % apply match rules to Json
                      true
                  end,
  case lists:search(SearchPattern, CompilePatterns) of
    false -> nomatch;
    {value, #{ <<"case">> := Case }} -> {match, Case}
  end.

%% --

compile_obj(MatchObj) ->
  maps:map(fun(_Prop,Pattern) ->
               build_pattern(Pattern)
           end, MatchObj).

build_pattern(Pattern) ->
  Rules = [check_type],
  % Fun = check_type,
  % Rules:Fun(Value),
  lists:map(Pattern, Rules).

check_type(#{ <<"type">> := <<"integer">> }, Value) ->
  is_integer(Value);
check_type(#{ <<"type">> := <<"boolean">> }, Value) ->
  is_boolean(Value).
