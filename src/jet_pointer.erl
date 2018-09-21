-module(jet_pointer).

-export([get/2, set/3, get_prop_value/2]).
-include_lib("eunit/include/eunit.hrl").


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

%%
%% pointer functions for nested-map-decoded JSON
%%

% gets a value at a specified property path in a nested map from decoded JSON
get_prop_value({pathList, [H | T]}, Json) when is_map(Json) ->
    % map, assume current path element is key in map
    case maps:find(H, Json) of
            {ok, Value} ->
                get_prop_value({pathList, T}, Value);
            error ->
                undefined
        end;
get_prop_value({pathList, [H | T]}, Json) when is_list(Json) ->
    {Index, _}=string:to_integer(H),
    % list, assume current path element is index of list element
    case (Index<0) of
        true ->
            get_prop_value({pathList, T}, lists:nth(length(Json)+Index+1, Json));
        false ->
            get_prop_value({pathList, T}, lists:nth(Index+1, Json))
    end;
get_prop_value({pathList, []}, Value) ->
    %we have reached the destination of the path, return value
    Value;
get_prop_value(PathString, Json) ->
    PathList = string:lexemes(PathString, "/"),
    get_prop_value({pathList, PathList}, Json).

% Returns a new nested map from an existing map with a property-path/value combination added:
% Use like this: Map = add_prop_value(PathString, Value, ExistingMap)
add_prop_value({pathList, [H | T]}, Value, Json) when is_map(Json) ->
    case (maps:is_key(H, Json)) of
        true ->
            maps:update(H, add_prop_value({pathList, T}, Value, maps:get(H, Json)), Json);
        false ->
            maps:put(H, add_prop_value({pathList, T}, Value, maps:new()), Json)
    end;
add_prop_value({pathList, _PathList}, _Value, Json) when is_list(Json) ->
    % cannot add specific array elements at specified index with this function
    % i.e. can't add_prop_value("/arrayproperty/2/propertyofarrayelement",Value,Map)
    {badpath};
add_prop_value({pathList, []}, Value, _Json) ->
    Value;
add_prop_value(PathString, Value, Map) ->
    PathList = string:lexemes(PathString, "/"),
    add_prop_value({pathList, PathList}, Value, Map).
