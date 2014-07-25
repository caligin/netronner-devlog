-module(achievements_application).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
   register(achievements_application, self()),
   supervisor:start_link({local, achievements_supervisor}, achievements_supervisor, []).

stop(_State) ->
    ok.