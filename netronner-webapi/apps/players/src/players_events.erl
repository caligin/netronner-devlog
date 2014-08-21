-module(players_events).
-export([start_link/0, notify/1, add_handler/1, add_handler/2]).
-export([achievement_award/2]).

%% event handling fns

start_link() ->
    gen_event:start_link({local, players_events}).

notify(Event) ->
    gen_event:notify(players_events, Event).

add_handler(Handler) ->
    add_handler(Handler, []).
add_handler(Handler, Args) ->
    gen_event:add_handler(players_events, Handler, Args).

%% event ctors
-spec achievement_award(player:player(), achievement:achievement()) -> {achievement_award, player:player(), achievement:achievement()}.
achievement_award(Player, Achievement) ->
    {achievement_award, Player, Achievement}.