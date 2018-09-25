-module(jet_pointer).

-export([get/2, set/3, get_prop_value/2, add_prop_value/3]).

-ifdef(TEST).
-compile(export_all).
-endif.


get("/", Json) ->
    Json;
get(<<"/">>, Json) ->
    Json;
get(Path, Json) ->
    get0(parse(Path), Json).


set("/", _Json, Value) ->
    Value;
set(<<"/">>, _Json, Value) ->
    Value;
set(Path, Json, Value) ->
    set0(parse(Path), Json, Value).


get0([], Json) ->
    Json;
get0([Key | RestPath], {Props}) ->
    case lists:keysearch(Key, 1, Props) of
        {value, {Key, Value}} ->
            get0(RestPath, Value);
        false ->
            throw({error, missing_path})
    end;
get0([Key | RestPath], Vals) when is_list(Vals) ->
    Idx = list_to_integer(binary_to_list(Key)),
    case Idx >= 0 andalso Idx =< length(Vals) of
        true ->
            get0(RestPath, lists:nth(Idx+1, Vals));
        _ ->
            throw({error, invalid_array_index})
    end;
get0(_, _) ->
    throw({error, missing_path}).


set0([], _Json, Value) ->
    Value;
set0(Path, {Props}, Value) ->
    set_prop(Path, Props, Value, []);
set0([Key | RestPath], Vals, Value) when is_list(Vals) ->
    Idx = list_to_integer(binary_to_list(Key)),
    case Idx >= 0 andalso Idx =< length(Vals) of
        true ->
            set_pos(RestPath, Vals, Value, Idx, []);
        _ ->
            throw({error, invalid_array_index})
    end;
set0(_, _, _) ->
    throw({error, missing_path}).


set_prop(_, [], _, _) ->
    throw({error, missing_path});
set_prop([Key | RestPath], [{Key, SubVal} | RestProps], Value, Acc) ->
    lists:reverse([{Key, set0(RestPath, SubVal, Value)} | Acc], RestProps);
set_prop(Path, [KV | RestProps], Value, Acc) ->
    set_prop(Path, RestProps, Value, [KV | Acc]).


set_pos(Path, [Val | RestVals], Value, 0, Acc) ->
    lists:reverse([set0(Path, Val, Value) | Acc], RestVals);
set_pos(Path, [Val | RestVals], Value, Idx, Acc) when Idx > 0 ->
    set_pos(Path, RestVals, Value, Idx-1, [Val | Acc]).


parse(Path) when is_list(Path) ->
    parse(list_to_binary(Path), []);
parse(Path) when is_binary(Path) ->
    parse(Path, []).


parse(<<>>, []) ->
    throw({error, empty_path});
parse(<<>>, [[] | _]) ->
    throw({error, trailing_slash});
parse(<<>>, [Last | Acc]) ->
    Finished = lists:reverse(Last),
    lists:map(fun list_to_binary/1, lists:reverse([Finished | Acc]));
parse(<<"/", Rest/binary>>, []) ->
    parse(Rest, [[]]);
parse(_, []) ->
    throw({error, invalid_path_start});
parse(<<"/", _Rest/binary>>, [[] | _]) ->
    throw({error, empty_path_component});
parse(<<"/", Rest/binary>>, [Acc | RestAcc]) ->
    parse(Rest, [[], lists:reverse(Acc) | RestAcc]);
parse(<<"%", H, L, Rest/binary>>, [Acc | RestAcc]) ->
    parse(Rest, [[dehex(H, L) | Acc] | RestAcc]);
parse(<<Val, Rest/binary>>, [Acc | RestAcc]) ->
    parse(Rest, [[Val | Acc] | RestAcc]).


dehex(H, L) ->
    (hexval(H) bsl 4) + hexval(L).


hexval(B) when (B >= 65 andalso B =< 70) ->
    B - 65 + 10;
hexval(B) when (B >= 97 andalso B =< 102) ->
    B - 97 + 10;
hexval(B) when (B >= 48 andalso B =< 57) ->
    B - 48.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                               %%
%% Pointer functions for nested-map-decoded JSON %%
%%                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% get_prop_value(Path, Json)
%% gets a value at a specified property path in a nested map from decoded JSON
%%

get_prop_value({path_list, [H | T]}, Json) when is_map(Json) ->
    % map, assume current path element is key in map
    case maps:find(H, Json) of
            {ok, Value} ->
                get_prop_value({path_list, T}, Value);
            error ->
                undefined
        end;
get_prop_value({path_list, [H | T]}, Json) when is_list(Json) ->
    {Index, _}=string:to_integer(H),
    % list, assume current path element is index of list element
    case (Index<0) of
        true ->
            get_prop_value({path_list, T}, lists:nth(length(Json)+Index+1, Json));
        false ->
            get_prop_value({path_list, T}, lists:nth(Index+1, Json))
    end;
get_prop_value({path_list, []}, Value) ->
    %we have reached the destination of the path, return value
    Value;
get_prop_value(PathString, Json) ->
    get_prop_value({path_list, string:lexemes(PathString, "/")}, Json).


%%
%% add_prop_value (Path, Value, Json)
%%
%% Used to iteratively build up a nested object
%% Returns a new nested map from an existing map with a property-path/value combination added:
%% Use like this: NewMap = add_prop_value(PathString, Value, ExistingMap).
%%
%% If a deep path is supplied and the required nesting properties do not exist they
%% will be created at the same time. So add_prop_value("/details/age",20,#{}) will
%% produce #{"details" => #{"age" => 20}}
%%
add_prop_value({path_list, [H | T]}, Value, Json) when is_map(Json) ->
    case (maps:is_key(H, Json)) of
        true ->
            maps:update(H, add_prop_value({path_list, T}, Value, maps:get(H, Json)), Json);
        false ->
            maps:put(H, add_prop_value({path_list, T}, Value, maps:new()), Json)
    end;
add_prop_value({path_list, path_list}, _Value, Json) when is_list(Json) ->
    % cannot add specific array elements at specified index with this function
    % i.e. can't add_prop_value("/arrayproperty/2/propertyofarrayelement",Value,Map)
    {throw(bad_path), path_list};
add_prop_value({path_list, []}, Value, _Json) ->
    Value;
add_prop_value(PathString, Value, Map) ->
    add_prop_value({path_list, string:lexemes(PathString, "/")}, Value, Map).
