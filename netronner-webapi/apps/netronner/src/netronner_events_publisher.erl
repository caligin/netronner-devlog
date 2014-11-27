-module(netronner_events_publisher).

-behaviour(gen_event).
-export([
    init/1,
    handle_event/2,
    handle_info/2,
    handle_call/2,
    code_change/3,
    terminate/2
    ]).
-export([
    achievement_award/2
    ]).

init([TimelineRepo]) ->
    {ok, TimelineRepo}.

handle_event({achievement_award, Player, Achievement}, TimelineRepo) ->
    Data = #{
        <<"player">> => #{
            <<"id">> => player:id(Player),
            <<"name">> => player:name(Player),
            <<"image_url">> => player:image_url(Player)
        },
        <<"achievement">> => #{
            <<"name">> => achievement:name(Achievement),
            <<"icon">> => achievement:icon(Achievement)
        }
    },
    Event = event:new(<<"achievement_award">>, Data),
    timeline:append(Event, TimelineRepo),
    {ok, TimelineRepo}.

handle_info(_Msg, State) ->
    {ok , State}.

handle_call(_Req, State) ->
    {ok, {error, badrequest}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

%% event ctors
-spec achievement_award(player:player(), achievement:achievement()) -> {achievement_award, player:player(), achievement:achievement()}.
achievement_award(Player, Achievement) ->
    {achievement_award, Player, Achievement}.