-module(netronner_handler_json).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [
        {<<"content-type">>, <<"application/json">>},
        {<<"access-control-allow-origin">>, <<"*">>},
        {<<"access-control-expose-headers">>, <<"www-authenticate">>}
    ]).
-define(DEFAULT_CORS_ALLOWED_HEADERS, "Origin, X-Requested-With, Content-Type, Accept").

init(_Type, Req, [Feature, Action]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {Feature, Action, Method}}.

handle(Req, {players, award_achievement, <<"OPTIONS">>}) -> %% CORS preflight
    CorsHeaders = cors_allow(["POST"],["Authorization"]),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS ++ CorsHeaders, Req),
    {ok, Req2, undefined};
handle(Req, {players, award_achievement, <<"POST">>}) ->
    case gen_aaa:chain(google_token, Req) of
        {ok, _} -> 
            {ok, RequestParams, Req2} = cowboy_req:body_qs(Req),
            {PlayerId, _} = cowboy_req:binding(player_id, Req),
            {_, AchievementName} = lists:keyfind(<<"achievement">>, 1, RequestParams),
            ok = players:award_achievement(PlayerId, AchievementName),
            {ok, Req3} = cowboy_req:reply(201, ?HEADERS, Req2),
            {ok, Req3, undefined};
        {error, Status, Headers} ->
            {ok, Req2} = cowboy_req:reply(Status, ?HEADERS ++ Headers, Req),
            {ok, Req2, undefined}
        end;
handle(Req, {_, _, Method }) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode({[{unsupported_method, Method}]}), Req),
    {ok, Req2, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.


cors_allow(Methods, Headers) ->
    [
        {<<"access-control-allow-methods">>, list_to_binary(string:join(Methods, ", "))},
        {<<"access-control-allow-headers">>, list_to_binary(string:join([?DEFAULT_CORS_ALLOWED_HEADERS | Headers], ", "))}
    ].
