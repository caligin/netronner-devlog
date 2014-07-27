-module(principal).

-export([make/3, id/1, name/1, image_url/1]).

-type principal() :: #{ id => binary(), displayName => binary(), image_url => binary()}.
-export_type([principal/0]).

-spec make(binary(), binary(), binary()) -> principal().
make(Id, DisplayName, ImageUrl) ->
    #{ 
        id => Id,
        displayName => DisplayName,
        image_url => ImageUrl
    }.

-spec id(principal()) -> binary().
id(Principal) ->
    maps:get(id, Principal).

-spec name(principal()) -> binary().
name(Principal) ->
    maps:get(displayName, Principal).

-spec image_url(principal()) -> binary().
image_url(Principal) ->
    maps:get(image_url, Principal).
