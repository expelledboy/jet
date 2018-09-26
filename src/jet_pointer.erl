-module(jet_pointer).

-export([get/2, put/3]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

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
get(PathString, Json) when is_binary(PathString)->
    get(string:lexemes(PathString, "/"), Json);
get([H | T], Json) when is_map(Json) ->
    % map, assume current path element is key in map
    case maps:find(H, Json) of
            {ok, Value} ->
                get(T, Value);
            error ->
                undefined
        end;
get([H | T], Json) when is_list(Json) ->
    {Index, _}=string:to_integer(H),
    case (Index<0) of
        true ->
            get(T, lists:nth(length(Json)+Index+1, Json));
        false ->
            get(T, lists:nth(Index+1, Json))
    end;
get([], Value) ->
    Value.

put(PathString, Value, Map) when is_binary(PathString) ->
    put(string:lexemes(PathString, "/"), Value, Map);
put([H | T], Value, Json) when is_map(Json) ->
    case (maps:is_key(H, Json)) of
        true ->
            maps:update(H, put(T, Value, maps:get(H, Json)), Json);
        false ->
            maps:put(H, put(T, Value, maps:new()), Json)
    end;
put(PathList, _Value, Json) when is_list(PathList), is_list(Json), length(PathList) > 0 ->
    {throw(bad_path), PathList};
put([], Value, _Json) ->
    Value.
