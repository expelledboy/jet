-module(jet_transformer).

-export([transform/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%
%% Exported transform function
%%

%% Exported transform() has an arity of 2 but internal functions
%% have an arity of 3 because they may be called from a "foreach"
%% in which case they must distinguish between relative paths within
%% the source array element being processed (ElementSource) or
%% absolute paths within the whole source Json object (Source)

transform(Transform, Source) when is_map(Transform) ->
    transform_map(Transform, Source, no_element).

transform_map(Transform, Source, ElementSource) when is_map(Transform) ->
    maps:fold(
            fun(Property, PropertyTransform, NewMap) ->
                jet_pointer:put(Property,
                    do_transform(PropertyTransform, Source, ElementSource),
                    NewMap)
            end,
            #{},
            Transform).

do_transform(Transform, Source, ElementSource) when is_map(Transform) ->
    %
    %  Keywords
    %
    % "properties" - Treat map value of destination property as top level transform,
    %                i.e. keys in map are destination properties of object value,
    %                not transform keywords
    % "foreach"    - Perform a transform on each element of a source array to
    %                produce elements in a destination array. Must be followed by
    %                "properties" keyword
    % "path"       - Currently used exclusively when we want to use a "convert"
    %                keyword, because a simple path string after a dest property in
    %                the transform is always treated as a pointer.
    %                When we want to convert values, we have to specify both
    %                a pointer "path" property and a "convert" property
    % "convert"    - Convert to a given format
    %

    case {maps:get(<<"properties">>, Transform, false),
          maps:get(<<"foreach">>, Transform, false),
          maps:get(<<"path">>, Transform, false),
          maps:get(<<"convert">>, Transform, false)} of
            {PropTransforms, false, false, false} when PropTransforms /= false ->
                transform_map(PropTransforms, Source, ElementSource);
            {PropTransforms, Foreach, false, false}
                when PropTransforms /= false, Foreach /= false  ->
                for_each(Foreach, PropTransforms, Source);
            {false, false, Path, Conversion} when Path /= false, Conversion /= false ->
                Value = do_transform(Path, Source, ElementSource),
                Fun = binary_to_existing_atom(Conversion, unicode),
                type:Fun(Value);
            {false, false, Path, false} when Path /= false ->
                do_transform(Path, Source, ElementSource);
            {false, false, false, false} ->
                throw({missing_transform_keywords, Transform})
    end;
do_transform(Transform, Source, ElementSource) when is_binary(Transform) ->
    {IsRelative, Pointer} = is_relative_pointer(Transform),
    case IsRelative of
        true ->
            jet_pointer:get(Pointer, ElementSource);
        false ->
            jet_pointer:get(Pointer, Source)
    end.

is_relative_pointer(Pointer) ->
    [Prefix|Path] = string:split(Pointer, <<"/">>, leading),
    case Prefix of
        <<"0">> ->
            {true, unicode:characters_to_binary([<<"/">>, Path], unicode)};
        _ ->
            {false, Pointer}
    end.

for_each (ArrayPointer, PropertyTransforms, Source) when is_binary(ArrayPointer),
                                                         is_map(PropertyTransforms) ->
    SourceArray=jet_pointer:get(
        ArrayPointer,
        Source),
    lists:map(fun(ElementSource) ->
            transform_map(PropertyTransforms, Source, ElementSource)
        end,
        SourceArray).

