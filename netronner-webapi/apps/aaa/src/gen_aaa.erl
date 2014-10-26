-module(gen_aaa).

-export([authenticate/2, principal/2, authorize/2]).

-callback authenticate(Req::cowboy_req:req()) -> ok | { error, StatusCode::number(), Headers::cowboy:http_headers()}.
-callback principal(Req::cowboy_req:req()) -> principal:principal().
-callback authorize(principal:principal()) -> boolean().

authenticate(Module, Req) ->
    Module:authenticate(Req).

principal(Module, Req) ->
    Module:principal(Req).

authorize(Module, Principal) ->
    Module:authorize(Principal).
