-module(netronner_repository_application).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
   register(netronner_repository_application, self()),
   supervisor:start_link({local, netronner_repository_supervisor}, netronner_repository_supervisor, []).

stop(_State) ->
    ok.