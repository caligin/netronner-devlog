-module(player).
-export([make/3, id/1, name/1, image_url/1, achievements/1, with_achievement/2]).

-type player() :: {Id::binary(), Name::binary(), ImageUrl::binary(), Achievements::[achievement:achievement()]}.
-export_type([player/0]).


-spec make(binary(), binary(), binary()) -> player().
make(Id, Name, ImageUrl) ->
    {Id, Name,  ImageUrl, []}.

-spec id(player()) -> binary().
id({Id, _, _, _}) ->
    Id.

-spec name(player()) -> binary().
name({_, Name, _, _}) ->
    Name.

-spec image_url(player()) -> binary().
image_url({_, _, ImageUrl, _}) ->
    ImageUrl.

-spec achievements(player()) -> [achievements:achievement()].
achievements({_, _, _, Achievements}) ->
    Achievements.

-spec with_achievement(achievements:achievement(), player()) -> player().
with_achievement(Achievement, {Id, Name, ImageUrl, Achievements}) ->
    case lists:any(fun(A) -> achievement:eq(A, Achievement) end, Achievements) of
        true -> {Id, Name, ImageUrl, Achievements};
        false -> {Id, Name, ImageUrl, [Achievement | Achievements]}
    end.
