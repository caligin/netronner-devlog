-module(gauth).
%% TODO make this a full-fledged application with a gen server and some caching of already seen tokens.
-export([validate_access_token/2, user_profile/1]).

-type user() :: #{ id => binary(), displayName => binary(), image_url => binary()}.
-export_type([user/0]).

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

-spec user_profile(AccessToken::binary()) -> user().
user_profile(AccessToken) ->
    {ok, Response} = httpc:request("https://www.googleapis.com/plus/v1/people/me?access_token=" ++ binary_to_list(AccessToken)),
    {{ _ProtoVersion, 200, _StatusMessage }, _Headers, Body} = Response,
    DecodedUserProfile = jiffy:decode(Body, [return_maps]),
    to_user(DecodedUserProfile).

%% TODO might want to trim the ?sz=50 appended at the end of image url to use different sizes...
-spec to_user(#{binary() => term()}) -> user().
to_user(Decoded) ->
    #{
        id => maps:get(<<"id">>, Decoded),
        displayName => maps:get(<<"displayName">>, Decoded),
        image_url => maps:get(<<"url">>, maps:get(<<"image">>, Decoded))
    }.


