-module(netronner_handler_json).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [
        {<<"content-type">>, <<"application/json">>},
        {<<"access-control-allow-origin">>, <<"*">>}
    ]).
-define(DEFAULT_CORS_ALLOWED_HEADERS, "Origin, X-Requested-With, Content-Type, Accept").

init(_Type, Req, [Feature, Action]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {Feature, Action, Method}}.

handle(Req, {timeline, page, <<"GET">>}) ->
    PageIndex = page_index_binding(Req),
    Page = timeline:page_to_dto(timeline:page(PageIndex)),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Page), Req),
    {ok, Req2, undefined};
handle(Req, {players, load, <<"GET">>}) ->
    {PlayerId, _} = cowboy_req:binding(player_id, Req),
    Player = player_to_json(players:load(PlayerId)),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, Player, Req),
    {ok, Req2, undefined};
handle(Req, {players, award_achievement, <<"OPTIONS">>}) -> %% CORS preflight
    CorsHeaders = cors_allow(["POST"],["Authorization"]),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS ++ CorsHeaders, Req),
    {ok, Req2, undefined};
handle(Req, {players, award_achievement, <<"POST">>}) ->
    case gen_aaa:authenticate(google_token, Req) of
        ok -> 
            true = gen_aaa:authorize(google_token, gen_aaa:principal(google_token, Req)),
            {ok, RequestParams, Req2} = cowboy_req:body_qs(Req),
            {PlayerId, _} = cowboy_req:binding(player_id, Req),
            {_, AchievementName} = lists:keyfind(<<"achievement">>, 1, RequestParams),
            ok = players:award_achievement(PlayerId, AchievementName),
            {ok, Req3} = cowboy_req:reply(201, ?HEADERS, Req2),
            {ok, Req3, undefined};
        {error, StatusCode, Headers} ->
            {ok, Req4} = cowboy_req:reply(StatusCode, Headers, Req),
            {ok, Req4, undefined}
        end;
handle(Req, {achievements, list_or_set, <<"GET">>}) ->
    Achievements = achievements:list(),
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, achievements_to_json(Achievements), Req),
    {ok, Req2, undefined};
handle(Req, {achievements, list_or_set, <<"PUT">>}) ->
    case gen_aaa:authenticate(administrative, Req) of
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


page_index_binding(Req) ->
    {PageIndexBin, _} = cowboy_req:binding(page, Req),
    case PageIndexBin of
        <<"latest">> -> latest;
        _ -> binary_to_integer(PageIndexBin)
    end.

cors_allow(Methods, Headers) ->
    [
        {<<"access-control-allow-methods">>, list_to_binary(string:join(Methods, ", "))},
        {<<"access-control-allow-headers">>, list_to_binary(string:join([?DEFAULT_CORS_ALLOWED_HEADERS | Headers], ", "))}
    ].

-spec achievement_to_map(achievement:achievement()) -> map().
achievement_to_map({Name, Description, Icon}) -> 
    #{
        <<"name">> => Name,
        <<"description">> => Description,
        <<"icon">> => Icon
    }.

-spec achievements_to_json([achievement:achievement()]) -> binary().
achievements_to_json(Achievements) when is_list(Achievements) ->
    AsMaps = lists:map(fun achievement_to_map/1, Achievements),
    jiffy:encode(AsMaps).

-spec player_to_json(player:player()) -> binary().
player_to_json({Id, Name, ImageUrl, Achievements}) ->
    AsMap = #{
            <<"id">> => Id,
            <<"name">> => Name,
            <<"imageUrl">> => ImageUrl,
            <<"achievements">> => lists:map(fun achievement_to_map/1, Achievements)
        },
    jiffy:encode(AsMap).

