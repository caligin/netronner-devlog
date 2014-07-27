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
    case gen_auth:is_authorized(google, Req) of
        ok -> 
            {ok, RequestParams, Req2} = cowboy_req:body_qs(Req),
            {_, PlayerName} = lists:keyfind(<<"player">>, 1, RequestParams),
            {_, AchievementName} = lists:keyfind(<<"achievement">>, 1, RequestParams),
            ok = players:award_achievement(PlayerName, AchievementName),
            {ok, Req3} = cowboy_req:reply(201, ?HEADERS, Req2),
            {ok, Req3, undefined};
        {error, StatusCode, Headers} ->
            {ok, Req4} = cowboy_req:reply(StatusCode, Headers, Req),
            {ok, Req4, undefined}
        end;
handle(Req, {players, login, <<"POST">>}) ->
    case gen_auth:is_authorized(google, Req) of
        ok ->
            Principal = gen_auth:principal(google, Req),
            Id = principal:id(Principal),
            Name = principal:name(Principal),
            IconUrl = principal:image_url(Principal),
            ok = players:add(players:new_player(Id, Name, IconUrl)),
            {ok, Req2} = cowboy_req:reply(201, ?HEADERS, Req),
            {ok, Req2, undefined};
        {error, StatusCode, Headers} ->
            {ok, Req3} = cowboy_req:reply(StatusCode, Headers, Req),
            {ok, Req3, undefined}
        end;
handle(Req, {achievements, list_or_set, <<"GET">>}) ->
    Achievements = achievements:list(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Achievements), Req),
    {ok, Req2, undefined};
handle(Req, {achievements, list_or_set, <<"PUT">>}) ->
    case gen_auth:is_authorized(administrative, Req) of
        ok ->
            {ok, Data, Req2} = cowboy_req:body(Req),
            Achievements = achievements:from_json(Data),
            ok = achievements:set(Achievements),
            {ok, Req3} = cowboy_req:reply(200, ?HEADERS, Req2),
            {ok, Req3, undefined};
        {error, StatusCode, Headers} ->
            {ok, Req4} = cowboy_req:reply(StatusCode, Headers, Req),
            {ok, Req4, undefined}
        end;
handle(Req, {_, _, Method }) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode({[{unsupported_method, Method}]}), Req),
    {ok, Req2, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.