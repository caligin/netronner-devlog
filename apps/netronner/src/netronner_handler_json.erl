-module(netronner_handler_json).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [{<<"content-type">>, <<"application/json">>}]).



init(_Type, Req, [Feature, Action]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {Feature, Action, Method}}.

handle(Req, {players, list, <<"GET">>}) ->
    Players = players:list(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Players), Req),
    {ok, Req2, undefined};
handle(Req, {players, award_achievement, <<"POST">>}) ->
    {PlayerName, Req2} = cowboy_req:qs_val(<<"player">>, Req),
    {AchievementName, Req3} = cowboy_req:qs_val(<<"achievement">>, Req2),
    ok = players:award_achievement(PlayerName, AchievementName),
    {ok, Req4} = cowboy_req:reply(201, ?HEADERS, Req3),
    {ok, Req4, undefined};
handle(Req, {achievements, list_or_set, <<"GET">>}) ->
    Achievements = achievements:list(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Achievements), Req),
    {ok, Req2, undefined};
handle(Req, {achievements, list_or_set, <<"PUT">>}) ->
    {ok, Data, Req2} = cowboy_req:body(Req),
    Achievements = jiffy:decode(Data, [return_maps]),
    %% TODO: achievements keys from binary to atom
    ok = achievements:set(Achievements),
    {ok, Req3} = cowboy_req:reply(200, ?HEADERS, Req2),
    {ok, Req3, undefined};
handle(Req, {_, _, Method }) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode({[{unsupported_method, Method}]}), Req),
    {ok, Req2, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.
