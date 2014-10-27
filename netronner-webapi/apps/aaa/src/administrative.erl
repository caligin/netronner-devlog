-module(administrative).
-behaviour(gen_aaa).

-export([authenticate/1, principal/1, authorize/1]).

authenticate(Req) ->
    {{Ip, _Port}, _} =  cowboy_req:peer(Req),
    case {127,0,0,1} =:= Ip of
        true -> ok;
        false -> {error, 403, []}
    end.

principal(_Req) ->
    principal:make(<<"noid">>, <<"Administrator">>).

authorize(_Principal) ->
    ok.