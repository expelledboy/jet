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

%%
%% Utility function broken out because code pattern is repeated twice -
%% in transform() and for_each() - and linter doesn't like code pattern
%% repetition
%%

transform_map(Transform, Source, ElementSource) when is_map(Transform) ->
    % recurse through transforms in current map, building
    % up a transformed object from Source or ElementSource (foreach)
    % using jet_pointer:add_prop_value() to create new dest object props
    maps:fold(
            fun(Property, PropertyTransform, NewMap) ->
                jet_pointer:add_prop_value(Property,
                    get_transform(PropertyTransform, Source, ElementSource),
                    NewMap)
            end,
            #{},
            Transform).

%%
%% Property transform function
%%

% Transform is map with keywords
get_transform(Transform, Source, ElementSource) when is_map(Transform) ->
    % these keywords should be mutually exclusive - i.e. we should
    % not find them together at the same nesting level of the
    % transform, hence ignoring various combinations in the case below
    %
    % "object"  -> Treat map value of destination property as top level transform,
    %              i.e. keys in map are destination properties of object value,
    %              not transform keywords
    % "foreach" -> Perform a transform on each element of a source array to
    %              produce elements in a destination array
    % "path"    -> Currently used exclusively when we want to use a "convert"
    %              keyword, because a simple path string after a dest property in
    %              the transform is always treated as a pointer.
    %              When we want to convert values, we have to specify both
    %              a pointer "path" property and a "convert" property
    case {maps:is_key(<<"object">>, Transform),
          maps:is_key(<<"foreach">>, Transform),
          maps:is_key(<<"path">>, Transform)} of
            {true, _, _} ->     %"object" keyword found
                transform_map(maps:get(<<"object">>, Transform), Source, ElementSource);
            {_, true, _} ->     %"foreach" keyword found
                for_each(maps:get(<<"foreach">>, Transform), Source);
            {_, _, true} ->     %path keyword found
                Value = get_transform(maps:get(<<"path">>, Transform), Source, ElementSource),
                case maps:find(<<"convert">>, Transform) of
                    {ok, Conversion} ->     % "convert" keyword in transform
                        Fun = binary_to_existing_atom(Conversion, unicode),
                        type:Fun(Value);
                    _ ->        % no "convert" keyword
                        Value
                end;
            {_, _, _} -> % transform is object, not simple JSON pointer (caught below),
                         % but no expected transform keywords were found
                throw({missing_transform_keywords, Transform})
    end;
% Transform is a simple JSON pointer string
get_transform(Transform, Source, ElementSource) when is_binary(Transform) ->
    {IsRelative, Pointer} = is_relative_pointer(Transform),
    case IsRelative of
        true ->
            jet_pointer:get_prop_value(Pointer, ElementSource);
        false ->
            jet_pointer:get_prop_value(Pointer, Source)
    end.



%%
%% Utility function to determine whether a Json Pointer is relative,
%% for array element transforms in foreach loops ("0/path"), or
%% absolute ("/path") - returns a tuple with a boolean (true for
%% relative, false for absolute) and a path with the leading 0
%% stripped out, if relative - because the calling functions want
%% to use it as an absolute path on the ElementSource they've been
%% handed.
%%

is_relative_pointer(Pointer) ->
    [Prefix|Path] = string:split(Pointer, <<"/">>, leading),
    case Prefix of
        <<"0">> -> % starts with zero, relative path to array element being transformed
            {true, unicode:characters_to_binary([<<"/">>, Path], unicode)};
        _ -> % absolute path to source object containing the array
            {false, Pointer}
    end.

%%
%% "foreach" keyword in transform
%% Note: nested "foreaches" not handled
%% So "foreach <element in A> -> foreach <element in element B in A>" is not allowed
%% - At least I don't think it will work, haven't actually run through the states
%% recursively to figure that out yet.
%%

for_each (Transform, Source) when is_map(Transform) ->
    case maps:is_key(<<"source-array">>, Transform) and maps:is_key(<<"properties">>, Transform) of
        true ->
            SourceArray=jet_pointer:get_prop_value(
                maps:get(<<"source-array">>, Transform),
                Source),
            PropertyTransforms= maps:get(<<"properties">>, Transform),
            lists:map(fun(ElementSource) ->
                    transform_map(PropertyTransforms, Source, ElementSource)
                end,
                SourceArray);
        _ -> %% necessary properties for foreach not supplied
            throw({missing_foreach_keywords, Transform})
    end.
