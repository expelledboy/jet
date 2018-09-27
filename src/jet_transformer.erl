-module(jet_transformer).

-export([transform/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

transform(Transform, Source) when is_map(Transform) ->
    transform_map(Transform, Source, no_element).

%% --

transform_map(Transform, Source, ElementSource) when is_map(Transform) ->
    %% XXX we distinguish relative paths with the ElementSource
    maps:fold(fun(Property, PropertyTransform, NewMap) ->
                      Value = do_transform(PropertyTransform, Source, ElementSource),
                      jet_pointer:put(Property, Value, NewMap)
              end, #{}, Transform).

%% --

do_transform(Transform, Source, ElementSource) when is_binary(Transform) ->
    case is_relative_pointer(Transform) of
        {true, Pointer} -> jet_pointer:get(Pointer, ElementSource);
        {false, Pointer} -> jet_pointer:get(Pointer, Source)
    end;

do_transform(#{ <<"foreach">> := ArrayPointer,
                <<"properties">> := PropTransforms },
             Source, _ElementSource) ->
    SourceArray = jet_pointer:get(ArrayPointer, Source),
    lists:map(fun(ElementSource) ->
                      transform_map(PropTransforms, Source, ElementSource)
              end, SourceArray);
do_transform(#{ <<"properties">> := PropTransforms }, Source, ElementSource) ->
    transform_map(PropTransforms, Source, ElementSource);
do_transform(#{ <<"path">> := Path } = Transform, Source, ElementSource) ->
    Value0 = do_transform(Path, Source, ElementSource),
    do_convertion(Value0, Transform).

%% --

do_convertion(Value, #{ <<"convert">> := Conversion }) ->
    Fun = binary_to_existing_atom(Conversion, unicode),
    case erlang:function_exported(type, Fun, 1) of
        true -> type:Fun(Value);
        false -> throw({conversion_not_supported, Conversion})
    end;
do_convertion(Value, _Transform) ->
    Value.

%% --

is_relative_pointer(Pointer) ->
    [Prefix|Path] = string:split(Pointer, <<"/">>, leading),
    case Prefix of
        <<"0">> -> {true, unicode:characters_to_binary([<<"/">>, Path], unicode)};
        _ -> {false, Pointer}
    end.

