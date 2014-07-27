-module(gen_auth).

-export([is_authorized/2, principal/2]).

-callback is_authorized(Req::cowboy_req:req()) -> ok | { error, StatusCode::number(), Headers::cowboy:http_headers()}.
-callback principal(Req::cowboy_req:req()) -> principal:principal().

is_authorized(Module, Req) ->
    Module:is_authorized(Req).

principal(Module, Req) ->
    Module:principal(Req).
