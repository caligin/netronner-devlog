-module(player).
-export([make/3, id/1, name/1, image_url/1, achievements/1, with_achievement/2]).

-type player() :: #{ id => binary(), name => binary(), image_url => binary(), achievements => [achievements:achievement()]}.
-export_type([player/0]).


-spec make(binary(), binary(), binary()) -> player().
make(Id, Name, ImageUrl) ->
    #{ 
        id => Id,
        name => Name,
        image_url => ImageUrl,
        achievements => []
    }.

-spec id(player()) -> binary().
id(Player) ->
    maps:get(id, Player).

-spec name(player()) -> binary().
name(Player) ->
    maps:get(displayName, Player).

-spec image_url(player()) -> binary().
image_url(Player) ->
    maps:get(image_url, Player).

-spec achievements(player()) -> [achievements:achievement()].
achievements(Player) ->
    maps:get(achievements, Player).

-spec with_achievement(achievements:achievement(), player()) -> player().
with_achievement(Achievement, Player) ->
    PlayerAchis = achievements(Player),
    %% FIXME: duplicates?
    maps:put(achievements, [Achievement | PlayerAchis], Player).
