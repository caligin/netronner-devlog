-module(principal).

-export([make/2, id/1, name/1]).

-type principal() :: #{ id => binary(), name => binary()}.
-export_type([principal/0]).

-spec make(binary(), binary()) -> principal().
make(Id, DisplayName) ->
    #{ id => Id, name => DisplayName }.

-spec id(principal()) -> binary().
id(Principal) ->
    maps:get(id, Principal).

-spec name(principal()) -> binary().
name(Principal) ->
    maps:get(name, Principal).
