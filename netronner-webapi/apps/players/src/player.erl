-module(player).
-export([new/3, id/1, name/1, image_url/1, achievements/1, with_achievement/2]).

-type player() :: {Id::binary(), Name::binary(), ImageUrl::binary(), Achievements::[achievement:achievement()]}.
-export_type([player/0]).


-spec new(binary(), binary(), binary()) -> player().
new(Id, Name, ImageUrl) ->
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

-spec achievements(player()) -> [achievement:achievement()].
achievements({_, _, _, Achievements}) ->
    Achievements.

-spec with_achievement(achievement:achievement(), player()) -> player().
with_achievement(Achievement, {Id, Name, ImageUrl, Achievements}) ->
    case lists:any(fun(A) -> achievement:eq(A, Achievement) end, Achievements) of
        true -> {Id, Name, ImageUrl, Achievements};
        false -> {Id, Name, ImageUrl, lists:sort(fun sort_by_achievement_name/2, [Achievement | Achievements])}
    end.

-spec sort_by_achievement_name(achievement:achievement(),achievement:achievement()) -> boolean().
sort_by_achievement_name({LhName, _ , _}, {RhName, _, _}) ->
    string:to_lower(binary_to_list(LhName)) =< string:to_lower(binary_to_list(RhName)).