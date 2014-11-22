-module(achievements).
-export([
    open/0,
    list/1,
    set/2,
    load/2,
    close/1
    ]).

-spec open() -> {ok, RepoHandle::atom()}.
open() ->
    dets:open_file(achievements,[]).

-spec list(RepoHandle::atom()) -> [achievement:achievement()].
list(RepoHandle) ->
    dets:foldl(fun(El, Memo) -> [El|Memo] end, [], RepoHandle).

-spec set([achievement:achievement()], RepoHandle::atom()) -> ok.
set(NewAchievements, RepoHandle) ->
    %% TODO: validation
    ok = dets:init_table(RepoHandle, fun(_) -> {NewAchievements, fun(_) -> end_of_input end} end),
    ok.

-spec load(AchievementName::binary(), RepoHandle::atom()) -> {ok, achievement:achievement()} | notfound.
load(AchievementName, RepoHandle) ->
    case dets:lookup(RepoHandle, AchievementName) of
        [Achievement] -> {ok, Achievement};
        [] -> notfound
    end.

-spec close(RepoHandle::atom()) -> ok | {error, term()}.
close(RepoHandle) ->
    dets:close(RepoHandle).