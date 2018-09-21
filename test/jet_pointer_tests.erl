-module(jet_pointer_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(MOD, jet_pointer).

pointer_test_() ->
    {ok, File} =
    file:read_file("test/fixtures/jet_pointer.json"),
    Suite = jiffy:decode(File, [return_maps]),
    lists:map(fun gen_pointer_tests/1, Suite).

gen_pointer_tests(Path) ->
    Type = maps:get(<<"test-type">>, Path),
    case Type of
        <<"get">> ->
            Object = maps:get(<<"object">>, Path),
            Pointers = maps:get(<<"pointers">>, Path),
            lists:map(fun(#{ <<"description">> := Description,
                            <<"path">> := Pointer,
                            <<"result">> := Expected }) ->
                        {Description,
                        ?_assertEqual(Expected, ((?MOD):get_prop_value(Pointer, Object)))}
                    end, Pointers);
         <<"add">> ->
            Objects = maps:get(<<"objects">>, Path),
            lists:map(fun(#{<<"description">> := Description,
                            <<"properties">> := Properties,
                            <<"result">> := Expected }) ->
                            {Description, ?_assertEqual(Expected, create_object(Properties))}
                      end, Objects)
    end.

create_object(Properties) ->
    lists:foldl(
        fun(Property, Map) ->
            Path = maps:get(<<"path">>, Property),
            Value = maps:get(<<"value">>, Property),
            (?MOD):add_prop_value(Path, Value, Map)
        end,
        maps:new(),
        Properties).
