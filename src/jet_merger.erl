-module(jet_merger).

-export([merge/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%
%% Merge spec supplied to merge() is simple map-decoded JSON object of the form
%% {
%%    "/path/to/property1": "merge"|"append"|"replace",
%%    "/path/to/property2": "merge"|"append"|"replace"
%% }
%%
%% Paths to individual array elements, like "/array/0" not allowed in the merge spec
%%
%% Default "merge" behaviour is applied to paths not specified in merge spec.
%% Merge instructions behave as follows, depending on the type at the path specified
%%
%% "merge"   - If primitive property, same as "replace"
%%             If array property, same as "append"
%%             If object property, replace existing properties & append new properties in
%%             dest object
%% "append"  - If primitive property, append only if property does not exist in dest
%%             If array property, append all array elements to dest array
%%             If object property, only append source object properties not found in dest object
%% "replace" - If property exists in dest, replace, otherwise append
%%
%% If a merge instruction is provided for a parent property, merge instructions for nested child
%% properties may be ignored when parent merge spec isn't "merge". e.g:
%% {
%%    "/path/to": "replace",
%%    "/path/to/property2": "append" <--- this is ignored, because the entire parent value "to" is
%%                                        replaced, including property2
%% }
%%

%%
%% Top-level objects must be maps (Json objects), not lists (Json arrays).
%%

%%
%% Internal functions take an additional "path" parameter which is updated
%% with the nested path during recursion across the source object. We
%% need this to get the matching properties from the dest object using
%% jet_pointer:get_prop_value()
%%

%%
%% Exported merge() function
%%

merge(_MergeSpec, Source, Dest) when is_map(Source), is_map(Dest), map_size(Dest) == 0 ->
    % Special case optimization:
    % Nothing in dest object so just return source object as merge result
    Source;
merge(MergeSpec, Source, Dest) when is_map(Source), is_map(Dest) ->
    merge(MergeSpec, Source, Dest, <<"/">>);
merge(_MergeSpec, Source, Dest) ->
     throw({not_json_objects, Source, Dest}).

%%
%% Internal functions
%%

%
% Merge/4
% Recurse into Source, passing  property path, whole Dest object,and flat merge
% specification so that we can use get_prop_value() and add_prop_value() to merge
% into Dest object.
%
merge(MergeSpec, Source, Dest, Path) when is_map(Source) ->
    case get_merge_type(MergeSpec, Path) of
        merge ->
            maps:fold(fun(K, V, Map)->
                        merge(MergeSpec, V, Map, append_to_path(Path, K))
                      end,
                      Dest,
                      Source);
        append ->
            maps:fold(fun(K, V, Map)->
                        merge(append, V, Map, append_to_path(Path, K))
                      end,
                      Dest,
                      Source);
        replace ->
            merge_property(replace, Source, Dest, Path)
    end;

merge(MergeSpec, Source, Dest, Path) ->
    merge_property(get_merge_type(MergeSpec, Path), Source, Dest, Path).

merge_property(MergeType, Source, Dest, Path) when is_list(Source) ->
    case jet_pointer:get_prop_value(Path, Dest) of
        undefined -> % add if prop does not exist in Dest
            jet_pointer:add_prop_value(Path, Source, Dest);
        Value -> % otherwise apply merge type
           case get_merge_type(MergeType, Path) of
                merge -> % append Source Value to Path in Dest
                    jet_pointer:add_prop_value(Path, lists:merge([Source, Value]), Dest);
                append -> % append Source Value to Path in Dest
                    jet_pointer:add_prop_value(Path, lists:merge([Source, Value]), Dest);
                replace -> % write Source Value over Path in Dest
                    jet_pointer:add_prop_value(Path, Source, Dest);
                MergeType ->
                    throw({invalid_merge_type, MergeType})
            end
    end;
merge_property(MergeType, Source, Dest, Path) ->
    case MergeType of
        merge -> % write Source Value over Path in Dest)
            jet_pointer:add_prop_value(Path, Source, Dest);
        replace -> % write Source Value over Path in Dest
            jet_pointer:add_prop_value(Path, Source, Dest);
        append ->
            case jet_pointer:get_prop_value(Path, Dest) of
                undefined -> % can't find property in dest, add it and return new dest
                    jet_pointer:add_prop_value(Path, Source, Dest);
                _ -> % found property, return unchanged dest object
                    Dest
            end;
        _ ->
            throw({invalid_merge_type, MergeType})
    end.

%
% merge types returned :    merge
%                           append
%                           replace
%
get_merge_type(MergeSpec, _Path) when is_atom(MergeSpec)->
    MergeSpec; % if this has been called with an existing spec atom just hand it back
               % this will happen when a parent property calls
get_merge_type(MergeSpec, Path) ->
    % check if we have a merge spec for current path
    case maps:find(Path, MergeSpec) of
        {ok, MergeType} -> % Merge spec exists for this path
            binary_to_existing_atom(MergeType, unicode);
        error -> % no merge spec exists for this path, return default merge spec
            merge
    end.

append_to_path(Path, Append) when Path == <<"/">> ->
    erlang:iolist_to_binary([Path, Append]);
append_to_path(Path, Append) ->
    erlang:iolist_to_binary([Path, <<"/">>, Append]).
