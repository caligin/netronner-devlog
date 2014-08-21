-module(netronner_events_publisher).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_info/2, handle_call/2, code_change/3, terminate/2]).
-record(state, { handler_ref }).

init([]) ->
    {ok, []}.

handle_event({achievement_award, Player, Achievement}, State) ->
    {PlayerId, Name,  ImageUrl, _} = Player,
    {AchievementName, _, Icon} = Achievement,
    Data = #{
        <<"player">> => #{
            <<"id">> => PlayerId,
            <<"name">> => Name,
            <<"image_url">> => ImageUrl
        },
        <<"achievement">> => #{
            <<"name">> => AchievementName,
            <<"icon">> => Icon
        }
    },
    Event = event:make(<<"achievement_award">>, Data),
    timeline:append(Event),
    {ok, State}.

handle_info(_Msg, State) ->
    {ok , State}.

handle_call(_Req, State) ->
    {ok, {error, badrequest}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.
