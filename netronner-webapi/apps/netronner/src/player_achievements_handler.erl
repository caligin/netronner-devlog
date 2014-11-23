-module(player_achievements_handler).

-export([
    init/3,
    rest_init/2,
    allowed_methods/2,
    is_authorized/2,
    options/2,
    resource_exists/2,
    allow_missing_post/2,
    content_types_accepted/2,
    award_achievement_json/2
    ]).

init(_Transport, Req, [_PlayersRepository, _AchievementsRepository, _EventBusRef] = Repositories) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {upgrade, protocol, cowboy_rest, Req2, Repositories}.

rest_init(Req, Repositories) ->
    {ok, Req, Repositories}.    

allowed_methods(Req, Repositories) ->
    {[<<"POST">>, <<"OPTIONS">>], Req, Repositories}.

is_authorized(Req, Repositories) ->
    {Method, _} = cowboy_req:method(Req),
    IsAuthorized = is_authorized(Req, Repositories, Method),
    Req2 = cowboy_req:set_resp_header(<<"access-control-expose-headers">>, <<"www-authenticate">>, Req),
    {IsAuthorized, Req2, Repositories}.

options(Req, Repositories) ->
    Headers = [
        {<<"access-control-allow-methods">>, <<"POST, OPTIONS">>},
        {<<"access-control-allow-headers">>, list_to_binary(["Origin, X-Requested-With, Content-Type, Accept, Authorization"])}
    ],
    Req2 = lists:foldl(fun({Header, Value}, ReqAcc) -> cowboy_req:set_resp_header(Header, Value, ReqAcc) end, Req, Headers),
    {ok, Req2, Repositories}.

resource_exists(Req, [PlayersRepository | _] = Repositories) ->
    {PlayerId, _} = cowboy_req:binding(player_id, Req),
    Exists = case players:load(PlayerId, PlayersRepository) of
        {ok, _} ->
            true;
        notfound ->
            ApiKey = google:api_key_make(element(2, application:get_env(netronner, google_api_key))),
            google:user_exists(ApiKey, PlayerId)
    end,
    {Exists, Req, Repositories}.

allow_missing_post(Req, Repositories) ->
    {false, Req, Repositories}.

content_types_accepted(Req, Repositories) ->
    {[{<<"application/json">>, award_achievement_json}], Req, Repositories}.

award_achievement_json(Req, [PlayersRepository, AchievementsRepository, EventBusRef] = Repositories) ->
    {PlayerId, _} = cowboy_req:binding(player_id, Req),
    {ok, Body, Req2} = cowboy_req:body(Req),
    IsOk = case decode_json(Body) of
        malformed ->
            false;
        {ok, AchievementName} ->
            case achievements:load(AchievementName, AchievementsRepository) of
                notfound ->
                    false;
                {ok, Achievement} ->
                    {ok, Added} = award_achievement(PlayerId, Achievement, PlayersRepository),
                    case Added of
                        false ->
                            ok;
                        true ->
                            {ok, Player} = players:load(PlayerId, PlayersRepository),
                            Event = netronner_events_publisher:achievement_award(Player, Achievement),
                            gen_event:notify(EventBusRef, Event)
                    end,
                    true
            end
    end,
    {IsOk, Req2, Repositories}.

%% priv
is_authorized(Req, [PlayersRepository | _], <<"POST">>) ->
    case google_token:is_authenticated(Req) of
        {false, _} = Res -> Res;
        true -> 
            is_principal_authorized(google_token:principal(Req), PlayersRepository)
    end;
is_authorized(_Req, _Repositories, _) ->
    true.

is_principal_authorized(Principal, PlayersRepository) ->
    case principal:id(Principal) of
        <<"108105329232958151930">> -> %% Caligin Tsukihara's google id.
            true;
        PrincipalId ->
            case players:load(PrincipalId, PlayersRepository) of
                {ok, _} -> true;
                notfound -> {false, <<"Bearer error=\"invalid_token\" error_description=\"unauthorized\"">>}
            end
    end.

-spec decode_json(JsonAchiName::binary()) -> {ok, binary()} | malformed.
%% expects {name:"name"}
decode_json(ArmoredName) ->
    AsMap = jiffy:decode(ArmoredName, [return_maps]),
    case AsMap of
        #{<<"name">> := Name} -> {ok, Name};
        _ -> malformed
    end.

award_achievement(PlayerId, Achievement, PlayersRepository) ->
    case players:load(PlayerId, PlayersRepository) of
        notfound -> add_player_from_google_info(PlayerId, PlayersRepository);
        {ok, _} -> ok
    end,
    players:award_achievement(PlayerId, Achievement, PlayersRepository).

add_player_from_google_info(PlayerId, PlayersRepository) ->
    ApiKey = google:api_key_make(element(2, application:get_env(netronner, google_api_key))),
    GoogleUser = google:user_profile(ApiKey, PlayerId),
    Player = user_to_player(GoogleUser),
    players:add(Player, PlayersRepository).

-spec user_to_player(google:user()) -> player:player().
user_to_player(GoogleUser) ->
    player:new(google:user_id(GoogleUser), google:user_name(GoogleUser), google:user_image_url(GoogleUser)). 