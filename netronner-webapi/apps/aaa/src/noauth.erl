-module(noauth).
-behaviour(gen_authentication).

-export([authenticate/1, principal/1]).

authenticate(_Req) ->
    ok.

principal(_Req) ->
    error(no_principal).
