-module(jet_pointer).

-export([get/2, get/3, put/3, remove/2]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

get(PathString, Json) when is_binary(PathString) ->
    get(string:lexemes(PathString, "/"), Json);

get([Key | Path], Json) when is_map(Json) ->
    case maps:find(Key, Json) of
        {ok, Value} -> get(Path, Value);
        error ->
            undefined
    end;
get([Key | Path], Json) when is_list(Json) ->
    case string:to_integer(Key) of
        {error, no_integer} ->
            throw({path_not_index, Key});
        {Index, _} when Index < 0 ->
            Value = lists:nth(length(Json)+Index+1, Json),
            get(Path, Value);
        {Index, _} ->
            Value = lists:nth(Index+1, Json),
            get(Path, Value)
    end;
get([_Key | Path], _Json) when is_list(Path), erlang:length(Path) == 0 ->
        undefined;
get(_Key, Value) ->
    Value.

get(PathString, Json, Default) when is_binary(PathString) ->
    case get(PathString,Json) of
        undefined -> Default;
        Value -> Value
    end.

put(PathString, Value, Map) when is_binary(PathString) ->
    put(string:lexemes(PathString, "/"), Value, Map);
put([Key | Path], Value, Json) when is_map(Json) ->
    case maps:is_key(Key, Json) of
        true ->
            NestedValue = put(Path, Value, maps:get(Key, Json)),
            maps:update(Key, NestedValue, Json);
        false ->
            NestedValue = put(Path, Value, maps:new()),
            maps:put(Key, NestedValue, Json)
    end;
put(PathList, _Value, Json) when is_list(PathList), is_list(Json), length(PathList) > 0 ->
    {throw(bad_path), PathList};
put([], Value, _Json) ->
    Value.

remove(PathString, Map) when is_binary(PathString) ->
    remove(string:lexemes(PathString, "/"), Map);
remove([Key | []], Json) when is_map(Json) ->
    case maps:is_key(Key, Json) of
        true ->
            maps:remove(Key,Json);
        false ->
            Json
    end;
remove([Key | Path], Json) when is_map(Json) ->
    case maps:is_key(Key, Json) of
        true ->
            NestedValue = remove(Path, maps:get(Key, Json)),
            maps:update(Key, NestedValue, Json);
        false ->
            Json
    end.

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
parse(Path, []) ->
    throw({invalid_path_start, Path});
parse(<<"/", _Rest/binary>>, [[] | _]) ->
    throw(empty_path_component);
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
