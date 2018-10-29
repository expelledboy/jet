-module(jet_pointer_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(MOD, jet_pointer).

pointer_test_() ->
    {ok, File} = file:read_file("test/fixtures/jet_pointer.json"),
    Suite = jiffy:decode(File, [return_maps]),
    lists:map(fun gen_pointer_tests/1, Suite).

gen_pointer_tests(Case) ->
    Type = maps:get(<<"test-type">>, Case),
    lists:map(fun({Desc, Test}) ->
                      {<<Type/binary, ": ", Desc/binary>>, Test}
              end, gen_pointer_tests(Type, Case)).

gen_pointer_tests(<<"get">>, Case) ->
    Object = maps:get(<<"object">>, Case),
    Pointers = maps:get(<<"pointers">>, Case),
    lists:map(fun(#{ <<"description">> := Desc,
                     <<"path">> := Pointer,
                     <<"result">> := Expected }) ->
                      Value = (catch ?MOD:get(Pointer, Object)),
                      {Desc, ?_assertEqual(Expected, Value)}
              end, Pointers);
gen_pointer_tests(<<"get-default">>, Case) ->
    Object = maps:get(<<"object">>, Case),
    Pointers = maps:get(<<"pointers">>, Case),
    lists:map(fun(#{ <<"description">> := Desc,
                     <<"path">> := Pointer,
                     <<"default">> := Default,
                     <<"result">> := Expected }) ->
                      Value = (catch ?MOD:get(Pointer, Object, Default)),
                      {Desc, ?_assertEqual(Expected, Value)}
              end, Pointers);
gen_pointer_tests(<<"put">>, Case) ->
    Objects = maps:get(<<"objects">>, Case),
    lists:map(fun(#{<<"description">> := Desc,
                    <<"properties">> := Properties,
                    <<"result">> := Expected }) ->
                      {Desc, ?_assertEqual(Expected, create_object(Properties))}
              end, Objects).

create_object(Properties) ->
    lists:foldl(fun(Property, Map) ->
                        Path = maps:get(<<"path">>, Property),
                        Value = maps:get(<<"value">>, Property),
                        ?MOD:put(Path, Value, Map)
                end, maps:new(), Properties).
