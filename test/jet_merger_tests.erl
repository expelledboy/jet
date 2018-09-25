-module(jet_merger_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(MOD, jet_merger).

merger_test_() ->
    {ok, File} =
    file:read_file("test/fixtures/jet_merger.json"),
    Suite = jiffy:decode(File, [return_maps]),
    lists:map(fun gen_merger_tests/1, Suite).

gen_merger_tests(Path) ->
    Source = maps:get(<<"source-object">>, Path),
    Merges = maps:get(<<"merges">>, Path),
    lists:map(fun(#{ <<"description">> := Description,
                    <<"dest-object">> := Dest,
                    <<"merge-spec">> := MergeSpec,
                    <<"result">> := Expected }) ->
                {Description,
                ?_assertEqual(Expected, ((?MOD):merge(MergeSpec, Source, Dest)))}
            end, Merges).

