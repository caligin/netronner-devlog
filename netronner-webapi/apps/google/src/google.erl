-module(google).
-export([user_id/1, user_name/1, user_image_url/1, user_decode/1]).
-export([access_token_make/1, api_key_make/1, typeof/1, token_value/1, to_qs/1]).
-export([validate_access_token/2, user_profile/2]).

-type user() :: #{ id => binary(), displayName => binary(), image_url => binary()}.
-export_type([user/0]).

-type token() :: {access_token|api_key, binary()}.
-export_type([token/0]).

% -----------------------------------------------------------------------------
% User manipulation fns
% -----------------------------------------------------------------------------
-spec user_make(binary(), binary(), binary()) -> user().
user_make(Id, DisplayName, ImageUrl) ->
    #{ 
        id => Id,
        displayName => DisplayName,
        image_url => ImageUrl
    }.

-spec user_id(user()) -> binary().
user_id(User) ->
    maps:get(id, User).

-spec user_name(user()) -> binary().
user_name(User) ->
    maps:get(displayName, User).

-spec user_image_url(user()) -> binary().
user_image_url(User) ->
    maps:get(image_url, User).

-spec user_decode(binary()) -> user().
user_decode(Encoded) ->
    Decoded = jiffy:decode(Encoded, [return_maps]),
    Id = maps:get(<<"id">>, Decoded),
    Name = maps:get(<<"displayName">>, Decoded),
    SizedImage = maps:get(<<"url">>, maps:get(<<"image">>, Decoded)),
    [Image | _ ] = re:split(SizedImage, "\\?"),
    user_make(Id, Name, Image).

% -----------------------------------------------------------------------------
% Token manipulation fns
% -----------------------------------------------------------------------------
-spec access_token_make(binary()) -> token().
access_token_make(AccessToken) ->
    {access_token, AccessToken}.

-spec api_key_make(binary()) -> token().
api_key_make(AccessToken) ->
    {api_key, AccessToken}.

-spec typeof(token()) -> access_token | api_key.
typeof({Type, _}) ->
    Type.

-spec token_value(token()) -> binary().
token_value({_, Value}) ->
    Value.

-spec to_qs(token()) -> binary().
to_qs({access_token, Value}) ->
    <<"access_token=", Value/binary>>;
to_qs({api_key, Value}) ->
    <<"key=", Value/binary>>.

% -----------------------------------------------------------------------------
% API fns
% -----------------------------------------------------------------------------

%% TODO consider using the key <<"expires_in">> => 3470 for caching control
-spec validate_access_token(AccessToken::token(), ClientId::binary()) -> ok | error.
validate_access_token({access_token, AccessToken}, ClientId) ->
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

-spec user_profile(token(), binary()) -> user().
user_profile(Token, UserId) ->
    {ok, Response} = httpc:request(
        "https://www.googleapis.com/plus/v1/people/"
        ++ binary_to_list(UserId)
        ++ "?"
        ++ binary_to_list(to_qs(Token))),
    {{ _ProtoVersion, 200, _StatusMessage }, _Headers, Body} = Response,
    DecodedUserProfile = jiffy:decode(Body, [return_maps]),
    toDO(DecodedUserProfile).

user_profile_request({access_token, TokenValue}, UserId) ->
    httpc:request("https://www.googleapis.com/plus/v1/people/" ++ UserId ++ "?access_token=" ++ binary_to_list(TokenValue));
user_profile_request({api_key, KeyValue}, UserId) ->
    httpc:request("https://www.googleapis.com/plus/v1/people/" ++ UserId ++ "?key=" ++ binary_to_list(KeyValue)).
    
