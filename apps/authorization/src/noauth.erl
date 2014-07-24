-module(noauth).
-behaviour(gen_auth).

-export([is_authorized/1, principal/1]).

is_authorized(_Req) ->
    ok.

principal(_Req) ->
    error(no_principal).
