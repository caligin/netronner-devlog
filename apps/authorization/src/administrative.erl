-module(administrative).
-behaviour(gen_auth).

-export([is_authorized/1, principal/1]).

is_authorized(Req) ->
    {{Ip, _Port}, _} =  cowboy_req:peer(Req),
    case {127,0,0,1} =:= Ip of
        true -> ok;
        false -> {error, 403, []}
    end.

principal(_Req) ->
    gen_auth:make_principal(<<"noid">>, <<"Administrator">>, <<"">>).
