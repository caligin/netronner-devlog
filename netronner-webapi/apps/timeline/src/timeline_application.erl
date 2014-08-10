-module(timeline_application).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
   register(timeline_application, self()),
   supervisor:start_link({local, timeline_supervisor}, timeline_supervisor, []).

stop(_State) ->
    ok.