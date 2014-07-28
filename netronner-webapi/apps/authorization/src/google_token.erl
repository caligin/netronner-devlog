-module(google_token).
-behaviour(gen_auth).

%% TODO make this a full-fledged application with a gen server and some caching of already seen tokens.
-export([is_authorized/1, principal/1]).

is_authorized(Req) ->
    Token = access_token_from_req(Req),
    {ok, <<ClientId/binary>>} = application:get_env(google_api_client_id),
    case google:validate_access_token(Token, ClientId) of
        ok -> true;
        error -> {error, 401, [{<<"WWW-Authenticate">>, <<"Bearer error=\"invalid_token\"">>}]}
    end.

principal(Req) ->
    Token = access_token_from_req(Req),
    GoogleUser = google:user_profile(Token, <<"me">>),
    user_to_principal(GoogleUser).

access_token_from_req(Req) -> 
    { <<"Bearer ", AccessToken/binary>>, _} = cowboy_req:header(<<"authorization">>, Req), %% FIXME this bombs on a malformed token, want to return 401...
    google:access_token_make(AccessToken).

user_to_principal(GoogleUser) ->
    Id = google:user_id(GoogleUser),
    Name = google:user_name(GoogleUser),
    principal:make(Id, Name).
