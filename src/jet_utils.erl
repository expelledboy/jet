-module(jet_utils).
-export([json_from_ref/1]).
-export([merge_lists/2, combine_lists/2, combine_maps/2]).

json_from_ref(Ref) ->
  case ref_type(Ref) of
    uri -> json_uri(Ref);
    file -> json_file(Ref)
  end.

json_uri(Uri) ->
  case httpc:request(get,{Uri,[]},[],[{body_format,binary}]) of
    {error, Error} -> throw({json_uri,Error,Uri});
    {ok,{{_,200,_},_Headers,Json}} -> parse_json(Json);
    {ok,{{_,HttpError,_},_Headers,_Body}} -> throw({json_uri,{http_error, HttpError},Uri})
  end.

json_file(Filename) ->
  case file:read_file(Filename) of
    {error, Error} -> throw({json_file,Error,Filename});
    {ok, Json} -> parse_json(Json)
  end.

parse_json(Json) ->
  Library = find_parser(),
  try
    {ok, Library:decode(Json, [return_maps])}
  catch
    {throw,{error,_} = Error} -> Error;
    _:E -> {error,E}
  end.

find_parser() ->
  case application:get_env(?MODULE, library, undefined) of
    undefined ->
      Modules = [code:ensure_loaded(jiffy), code:ensure_loaded(jsx)],
      Library = proplists:get_value(module, Modules),
      ok = application:set_env(?MODULE, library, Library),
      Library;
    Library -> Library
  end.

ref_type(Ref) ->
  case http_uri:parse(Ref) of
    {ok, _} -> uri;
    _ -> file
  end.

merge_lists(Parent,Child) ->
  Combined = Parent ++ Child,
  Fun = fun(Key) ->
            [Value|_] = proplists:get_all_values(Key,Combined),
            {Key,Value}
        end,
  lists:map(Fun,proplists:get_keys(Combined)).

combine_lists(Parent,Child) when is_list(Parent), is_list(Child) ->
  Combined = Parent ++ Child,
  Fun = fun(Key) ->
            Values = proplists:get_all_values(Key,Combined),
            case combine_values(Values) of
              badarg -> throw({additional_values_found, Key, Values});
              Value -> {Key, Value}
            end
        end,
  lists:map(Fun,proplists:get_keys(Combined)).

combine_maps(Parent,Child) when is_map(Parent), is_map(Child) ->
  Combined = maps:merge(Child, Parent),
  Fun = fun(K,V1) ->
            case maps:get(K, Child, nil) of
              nil -> V1;
              V2 -> combine_values([V1, V2])
            end
        end,
  maps:map(Fun,Combined).

combine_values(Values) ->
  case Values of
    [L1, L2] when is_list(L1), is_list(L2) -> L1 ++ L2;
    [L1, V2] when is_list(L1) -> L1 ++ [V2];
    [V1, L2] when is_list(L2) -> [V1|L2];
    [V1, V2] -> [V1, V2];
    [V] -> V;
    Vs when length(Vs) > 2 -> badarg
  end.
