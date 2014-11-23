-module(google_token).

-export([is_authenticated/1, principal/1]).

-spec is_authenticated(cowboy_req:req()) -> true | {false, binary}.
is_authenticated(Req) ->
    is_authenticated(Req, extract_maybe_token(Req)).

is_authenticated(_Req, []) ->
    {false, <<"Bearer realm=\"netronner\"">>};
is_authenticated(_Req, [Token]) ->
    {ok, <<ClientId/binary>>} = application:get_env(aaa, google_api_client_id),
    case google:validate_access_token(Token, ClientId) of
        ok -> true;
        error -> {false, <<"Bearer error=\"invalid_token\"">>}
    end.

-spec principal(cowboy_req:req()) -> principal:principal().
principal(Req) ->
    [Token] = extract_maybe_token(Req),
    GoogleUser = google:user_profile(Token, <<"me">>),
    user_to_principal(GoogleUser).

extract_maybe_token(Req) -> 
    case cowboy_req:header(<<"authorization">>, Req) of
        { undefined, _} -> [];
        { <<"Bearer ", AccessToken/binary>>, _} -> [google:access_token_make(AccessToken)]
    end.

user_to_principal(GoogleUser) ->
    Id = google:user_id(GoogleUser),
    Name = google:user_name(GoogleUser),
    principal:make(Id, Name).
