-module(noauth).
-behaviour(gen_aaa).

-export([authenticate/1, principal/1, authorize/1]).

authenticate(_Req) ->
    ok.

principal(_Req) ->
    error(no_principal).

authorize(_Principal) ->
    true.