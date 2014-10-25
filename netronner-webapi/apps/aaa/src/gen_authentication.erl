-module(gen_authentication).

-export([authenticate/2, principal/2]).

-callback authenticate(Req::cowboy_req:req()) -> ok | { error, StatusCode::number(), Headers::cowboy:http_headers()}.
-callback principal(Req::cowboy_req:req()) -> principal:principal().

authenticate(Module, Req) ->
    Module:authenticate(Req).

principal(Module, Req) ->
    Module:principal(Req).
