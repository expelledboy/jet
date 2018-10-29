-module(jet_matcher_tests).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(MOD, jet_matcher).

match_test_() ->
    {ok, File} = file:read_file("test/fixtures/jet_matcher.json"),
    Suite = jiffy:decode(File, [return_maps]),
    lists:map(fun gen_pattern_tests/1, Suite).

gen_pattern_tests(Case) ->
    Object = maps:get(<<"object">>, Case),
    Patterns = maps:get(<<"patterns">>, Case),
    lists:map(fun(#{ <<"description">> := Description,
                     <<"pattern">> := Pattern } = Test) ->
                      Expected = maps:get(<<"result">>, Test, true),
                      test(Description, Expected, Pattern, Object)
              end, Patterns).

test(Description, Expected, Pattern, Object) ->
    Match = (catch ?MOD:match(Pattern, Object)),
    {Description, ?_assertEqual(Expected, Match)}.
