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

%%
%% The jet_pointer function add_prop_value takes a supplied object and adds
%% both the property specified and it's value to the object, creating or updating any nested
%% properties required if a deep path to the property is supplied (e.g /details/age)
%% It is mainly used by the transformer and merge code to create new objects iteratively
%% so the tests construct objects by successively adding properties, hence this function
%%

create_object(Properties) ->
    lists:foldl(
        fun(Property, Map) ->
            Path = maps:get(<<"path">>, Property),
            Value = maps:get(<<"value">>, Property),
            (?MOD):add_prop_value(Path, Value, Map)
        end,
        maps:new(),
        Properties).
