-module(google_token).
-behaviour(gen_auth).

%% TODO make this a full-fledged application with a gen server and some caching of already seen tokens.
-export([is_authorized/1, principal/1]).

is_authorized(Req) ->
    { <<"Bearer ", AccessToken/binary>>, Req2} = cowboy_req:header(<<"authorization">>, Req), %% FIXME this bombs on a malformed token, want to return 401...
    {ok, <<ClientId/binary>>} = application:get_env(google_api_client_id),
    case validate_access_token(AccessToken, ClientId) of
        ok -> true;
        error -> {error, 401, [{<<"WWW-Authenticate">>, <<"Bearer error=\"invalid_token\"">>}]}
    end.

principal(Req) ->
    { <<"Bearer ", AccessToken/binary>>, _} = cowboy_req:header(<<"authorization">>, Req),
    user_profile(AccessToken).

%% TODO consider using the key <<"expires_in">> => 3470 for caching control
-spec validate_access_token(AccessToken::binary(), ClientId::binary()) -> ok | error.
validate_access_token(AccessToken, ClientId) ->
    {ok, Response} = httpc:request("https://www.googleapis.com/oauth2/v1/tokeninfo?access_token=" ++ binary_to_list(AccessToken)),
    {{ _ProtoVersion, StatusCode, _StatusMessage }, _Headers, Body} = Response,
    case StatusCode of
        200 -> check_token_ownership(Body, ClientId);
        400 -> error
    end.

-spec check_token_ownership(BinaryTokenInfo::binary(), ClientId::binary()) -> ok | error.
check_token_ownership(BinaryTokenInfo, ClientId) ->
    TokenInfo = jiffy:decode(BinaryTokenInfo, [return_maps]),
    case ClientId =:= maps:get(<<"issued_to">>, TokenInfo) of
        true -> ok;
        false -> error
    end.

-spec user_profile(AccessToken::binary()) -> gen_auth:principal().
user_profile(AccessToken) ->
    {ok, Response} = httpc:request("https://www.googleapis.com/plus/v1/people/me?access_token=" ++ binary_to_list(AccessToken)),
    {{ _ProtoVersion, 200, _StatusMessage }, _Headers, Body} = Response,
    DecodedUserProfile = jiffy:decode(Body, [return_maps]),
    to_principal(DecodedUserProfile).

-spec to_principal(#{binary() => term()}) -> principal:principal().
to_principal(Decoded) ->
    Id = maps:get(<<"id">>, Decoded),
    Name = maps:get(<<"displayName">>, Decoded),
    Image = maps:get(<<"url">>, maps:get(<<"image">>, Decoded)),
    principal:make(Id, Name).


