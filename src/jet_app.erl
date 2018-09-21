-module(jet_app).
-behaviour(application).

-export([ start/2, stop/1 ]).

start(_Type, _StartArgs) ->
    ok = jet_cache:install().

stop(_State) ->
    ok.
