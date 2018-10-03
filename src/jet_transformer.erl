-module(jet_transformer).

-export([transform/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

transform(Transform, Source) when is_map(Transform) ->
    transform_map(Transform, Source, no_element).

%% --

transform_map(Transform, Source, ElementSource) when is_map(Transform) ->
    maps:fold(fun(Property, PropertyTransform, NewMap) ->
                      Value = do_transform(PropertyTransform, Source, ElementSource),
                      jet_pointer:put(Property, Value, NewMap)
              end, #{}, Transform).

%% --

do_transform(Transform, Source, ElementSource) when is_binary(Transform) ->
    %% XXX we distinguish relative paths with the ElementSource
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
    case (Value0==undefined) of
        true ->
            case (maps:get(<<"default">>,Transform)) of
                {badkey,<<"default">>} ->
                    Value1 = Value0;
                Default ->
                    Value1 = Default
            end,
            do_function(Value1, Transform);
        false ->
            do_function(Value0, Transform)
    end;
do_transform(#{ <<"default">> := Default } = Transform, _Source, _ElementSource) ->
    do_function(Default, Transform).
%% --

do_function(Value, #{ <<"transform">> := Function }) ->
    Fun = binary_to_existing_atom(Function, unicode),
    case erlang:function_exported(jet_functions, Fun, 1) of
        true -> jet_functions:Fun(Value);
        false -> throw({conversion_not_supported, Function})
    end;
do_function(Value, _Transform) ->
    Value.

%% --

is_relative_pointer(Pointer) ->
    [Prefix|Path] = string:split(Pointer, <<"/">>, leading),
    case Prefix of
        <<"0">> -> {true, unicode:characters_to_binary([<<"/">>, Path], unicode)};
        _ -> {false, Pointer}
    end.

