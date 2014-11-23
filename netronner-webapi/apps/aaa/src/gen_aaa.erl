-module(gen_aaa).

-export([authenticate/2, principal/2, authorize/2, chain/2]).

-callback authenticate(Req::cowboy_req:req()) -> ok | { error, StatusCode::number(), Headers::cowboy:http_headers()}.
-callback principal(Req::cowboy_req:req()) -> principal:principal().
-callback authorize(principal:principal()) -> ok | { error, StatusCode::number(), Headers::cowboy:http_headers()}.

authenticate(Module, Req) ->
    Module:authenticate(Req).

principal(Module, Req) ->
    Module:principal(Req).

authorize(Module, Principal) ->
    Module:authorize(Principal).

-spec chain(atom(), cowboy_req:req()) -> {ok, principal:principal()} | { error, StatusCode::number(), Headers::cowboy:http_headers()}.
chain(AuthModule, Req) ->
    case AuthModule:authenticate(Req) of
        ok ->
            Principal = AuthModule:principal(Req),
            case AuthModule:authorize(Principal) of
                ok ->
                    {ok, Principal};
                {error, _Status, _Headers} = Result-> 
                    Result
            end;        
        {error, _Status, _Headers} = Result->
            Result
    end.

