-module(players_application).
-behaviour(application).
-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
   register(players_application, self()),
   supervisor:start_link({local, players_supervisor}, players_supervisor, []).

stop(_State) ->
    ok.