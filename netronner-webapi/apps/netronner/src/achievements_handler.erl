-module(achievements_handler).
-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    forbidden/2,
    content_types_provided/2,
    content_types_accepted/2,
    get_achievements_json/2,
    put_achievements_json/2
    ]).

init(_Transport, Req, [Repository]) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {upgrade, protocol, cowboy_rest, Req2, Repository}.

rest_init(Req, Repository) ->
    {ok, Req, Repository}.

allowed_methods(Req, Repository) ->
    {[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"OPTIONS">>], Req, Repository}.

forbidden(Req, Repository) ->
    Forbidden = case cowboy_req:method(Req) of
        {<<"PUT">>, _} -> not request_is_administrative(Req);
        {_OtherMethod, _} -> false
    end,
    {Forbidden, Req, Repository}.

content_types_provided(Req, Repository) ->
    {[{<<"application/json">>, get_achievements_json}], Req, Repository}.

content_types_accepted(Req, Repository) ->
    {[{<<"application/json">>, put_achievements_json}], Req, Repository}.

get_achievements_json(Req, Repository) ->
    Achievements = achievements:list(Repository),
    {encode_json(Achievements), Req, Repository}.

put_achievements_json(Req, Repository) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    NewAchievements = decode_json(Body),
    ok = achievements:set(NewAchievements, Repository),
    {true, Req2, Repository}.

%% private

request_is_administrative(Req) ->
    {{Ip, _Port}, _} =  cowboy_req:peer(Req),
    {127,0,0,1} =:= Ip.

-spec encode_json([achievement:achievement()]) -> binary().
encode_json(Achievements) when is_list(Achievements) ->
    AsMaps = lists:map(fun to_map/1, Achievements),
    jiffy:encode(AsMaps).

-spec to_map(achievement:achievement()) -> map().
to_map(Achi) -> 
    #{
        <<"name">> => achievement:name(Achi),
        <<"description">> => achievement:description(Achi),
        <<"icon">> => achievement:icon(Achi)
    }.

-spec decode_json(JsonAchievements::binary()) -> [achievement:achievement()].
decode_json(JsonAchievements) ->
    BinaryAchis = jiffy:decode(JsonAchievements, [return_maps]),
    decode_maps(BinaryAchis, []).

decode_maps([BinaryAchievement | Others], Converted) ->
    #{
        <<"name">> := Name,
        <<"description">> := Description,
        <<"icon">> := Icon
    } = BinaryAchievement,
    Achievement = achievement:new(Name, Description, Icon),
    decode_maps(Others, [Achievement | Converted]);
decode_maps([], Converted) ->
    Converted.
