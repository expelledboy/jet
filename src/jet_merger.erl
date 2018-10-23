-module(jet_merger).

-include_lib("eunit/include/eunit.hrl").

-export([merge/3]).

-ifdef(TEST).
-compile(export_all).
-endif.

merge(MergeSpec, Source, Dest) when is_map(MergeSpec) ->
    CleanedSource = merge_delete(MergeSpec, Source),
    CleanedDest = merge_delete(MergeSpec, Dest),
    Path = <<"/">>,
    MergeType = get_merge_type(MergeSpec, Path),
    merge(MergeType, MergeSpec, CleanedSource, CleanedDest, Path).
merge(append, MergeSpec, Source, Dest, Path) when is_map(Source), is_map(Dest) ->
    maps:fold(fun(Prop, PropValue, DestAcc)->
                      PropPath = append_to_path(Path, Prop),
                      merge(append, MergeSpec, PropValue, DestAcc, PropPath)
              end, Dest, Source);

merge(append, _MergeSpec, Source, Dest, Path) ->
    case jet_pointer:get(Path, Dest) of
        undefined ->
            jet_pointer:put(Path, Source, Dest);
        Value when is_list(Source), is_list(Value) ->
            jet_pointer:put(Path, lists:merge([Source, Value]), Dest);
        _Value ->
            Dest
    end;
merge(replace, _MergeSpec, Source, Dest, Path) ->
    jet_pointer:put(Path, Source, Dest);
merge(merge, MergeSpec, Source, Dest, Path) ->
    case jet_pointer:get(Path, Dest) of
        undefined ->
            merge(replace, MergeSpec, Source, Dest, Path);
        Value when is_list(Source), is_list(Value) ->
            merge(append, MergeSpec, Source, Dest, Path);
        Value when is_map(Source), is_map(Value) ->
            maps:fold(fun(Prop, PropValue, DestAcc)->
                              PropPath = append_to_path(Path, Prop),
                              MergeType = get_merge_type(MergeSpec, PropPath),
                              merge(MergeType, MergeSpec, PropValue, DestAcc, PropPath)
                      end, Dest, Source);
        _Value ->
            merge(replace, MergeSpec, Source, Dest, Path)
    end.

%% --

merge_delete(MergeSpec,Dest) ->
    maps:fold(
        fun(K,V,Acc) ->
            case V == <<"delete">> of
                true -> jet_pointer:remove(K, Acc);
                false -> Acc
            end
        end,
        Dest,
        MergeSpec
    ).

get_merge_type(MergeSpec, _Path) when is_atom(MergeSpec)->
    MergeSpec;
get_merge_type(MergeSpec, Path) ->
    MergeType = maps:get(Path, MergeSpec, <<"merge">>),
    binary_to_existing_atom(MergeType, unicode).

append_to_path(Path, Append) when Path == <<"/">> ->
    erlang:iolist_to_binary([Path, Append]);
append_to_path(Path, Append) ->
    erlang:iolist_to_binary([Path, <<"/">>, Append]).

