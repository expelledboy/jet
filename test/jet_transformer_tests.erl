-module(jet_transformer_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(MOD, jet_transformer).

transformer_test_() ->
    {ok, File} =
    file:read_file("test/fixtures/jet_transformer.json"),
    Suite = jiffy:decode(File, [return_maps]),
    lists:map(fun gen_transformer_tests/1, Suite).

gen_transformer_tests(Path) ->
    Object = maps:get(<<"object">>, Path),
    Transforms = maps:get(<<"transforms">>, Path),
    lists:map(fun(#{ <<"description">> := Description,
                    <<"transform">> := Transform,
                    <<"result">> := Expected }) ->
                {Description,
                ?_assertEqual(Expected, ((?MOD):transform(Transform, Object)))}
            end, Transforms).

