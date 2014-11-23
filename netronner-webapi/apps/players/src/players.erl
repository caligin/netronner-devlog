-module(players).

-export([
    open/0,
    open/1,
    load/2,
    add/2,
    award_achievement/3,
    close/1
    ]).

-spec open() -> {ok, RepoHandle::atom()}.
open() ->
    dets:open_file(?MODULE,[]).

-spec open(BasePath::string()) -> {ok, RepoHandle::atom()}.
open(BasePath) ->
    dets:open_file(?MODULE,[{file, [BasePath, $/, atom_to_list(?MODULE)]}]).

-spec load(PlayerId::binary(), RepoHandle::atom()) -> {ok, player:player()} | notfound.
load(PlayerId, RepoHandle) ->
    case dets:lookup(RepoHandle, PlayerId) of
        [Player] -> {ok, Player};
        [] -> notfound
    end.

-spec add(player:player(), RepoHandle::atom()) -> ok.
add(Player, RepoHandle) ->
    case dets:insert_new(RepoHandle, Player) of
        true -> ok;
        false -> ok
    end.

%% TODO: we have a race condition here. I'm going to ignore is for now as the chances of it happening with the initial user base are extremely low.
%% TODO: this addedd boolean is horrible, it's a fast hack to fix the duplicated event bug. refactor to have a gen_event. This is one of the reasons why
%%        having apps instead of lib apps is good.
-spec award_achievement(PlayerId::binary(), achievement:achievement(), RepoHandle::atom()) -> {ok, Added::boolean()}.
award_achievement(PlayerId, Achievement, RepoHandle) ->
    [OldPlayer] = dets:lookup(RepoHandle, PlayerId),
    UpdatedPlayer = player:with_achievement(Achievement, OldPlayer),
    ok = dets:insert(RepoHandle, UpdatedPlayer),
    {ok, OldPlayer =/= UpdatedPlayer}.

-spec close(RepoHandle::atom()) -> ok.
close(RepoHandle) ->
    ok = dets:close(RepoHandle).
