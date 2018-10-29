-module(jet_transformer).

-export([transform/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

transform(Transform, Source) when is_map(Transform) ->
    transform_map(Transform, Source, no_element).

%% --

transform_map(#{ <<"case">> := Case }, Source, ElementSource) ->
    transform_case(Case, Source, ElementSource);
transform_map(Transform, Source, ElementSource) when is_map(Transform) ->
    maps:fold(fun(Property, PropertyTransform, NewMap) ->
                      Value = transform_prop(PropertyTransform, Source, ElementSource),
                      jet_pointer:put(Property, Value, NewMap)
              end, #{}, Transform).

%% --

transform_prop(Transform, Source, ElementSource) when is_binary(Transform) ->
    %% XXX we distinguish relative paths with the ElementSource
    case is_relative_pointer(Transform) of
        {true, Pointer} -> jet_pointer:get(Pointer, ElementSource);
        {false, Pointer} -> jet_pointer:get(Pointer, Source)
    end;
transform_prop(#{ <<"foreach">> := ArrayPointer,
                  <<"properties">> := PropTransforms },
               Source, _ElementSource) ->
    SourceArray = jet_pointer:get(ArrayPointer, Source),
    lists:map(fun(ElementSource) ->
                      transform_map(PropTransforms, Source, ElementSource)
              end, SourceArray);
transform_prop(#{ <<"case">> := Case }, Source, ElementSource) when is_list(Case)->
    transform_case(Case, Source, ElementSource);
transform_prop(#{ <<"properties">> := PropTransforms }, Source, ElementSource) ->
    transform_map(PropTransforms, Source, ElementSource);
transform_prop(#{ <<"path">> := Path } = Transform, Source, ElementSource) ->
    Default = maps:get(<<"default">>, Transform, undefined),
    case transform_prop(Path, Source, ElementSource) of
        undefined when Default =:= undefined -> throw({path_not_found, Path});
        undefined -> do_function(Default, Transform);
        Value -> do_function(Value, Transform)
    end;
transform_prop(#{ <<"default">> := Default } = Transform, _Source, _ElementSource) ->
    do_function(Default, Transform);
transform_prop(Value, _Source, _ElementSource) ->
    Value.

%% --

transform_case([#{<<"pattern">> := Pattern} = Match| Tail], Source, ElementSource) ->
    case jet_matcher:match(Pattern, Source) of
        true -> transform_prop(Match, Source, ElementSource);
        false -> transform_case(Tail, Source, ElementSource)
    end;
transform_case([MatchElse| _Tail], Source, ElementSource) ->
    transform_prop(MatchElse, Source, ElementSource);
transform_case([], _Source, _ElementSource) ->
    throw(case_clause).

%% --

do_function(Value, #{ <<"transform">> := Function }) ->
    Fun = binary_to_existing_atom(Function, unicode),
    case erlang:function_exported(jet_functions, Fun, 1) of
        true -> jet_functions:Fun(Value);
        false -> throw({transform_not_supported, Function})
    end;
do_function(Value, _Transform) ->
    Value.

is_relative_pointer(Pointer) ->
    [Prefix|Path] = string:split(Pointer, <<"/">>, leading),
    case Prefix of
        <<"0">> -> {true, unicode:characters_to_binary([<<"/">>, Path], unicode)};
        _ -> {false, Pointer}
    end.

