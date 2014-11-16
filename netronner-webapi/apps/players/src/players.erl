-module(players).

-behaviour(gen_server).
-export([
    open/0,
    load/2,
    merge/2,
    close/1
    ]).
-export([award_achievement/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec award_achievement(PlayerId::binary(), AchievementName::binary()) -> ok.
award_achievement(PlayerId, AchievementName) ->
    gen_server:call(?MODULE, {award_achievement, PlayerId, AchievementName }).


init([]) ->
    %%TODO: configurable filename
    dets:open_file(players,[]),
    {ok, #{}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({award_achievement, PlayerId, AchievementName}, _From, State) ->
    ok = award_achievement(State, PlayerId, AchievementName),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, badreqeust}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok = dets:close(players),
    ok.

-spec open() -> {ok, RepoHandle::atom()}.
open() ->
    dets:open_file(players,[]).

-spec load(PlayerId::binary(), RepoHandle::atom()) -> {ok, player:player()} | notfound.
load(PlayerId, RepoHandle) ->
    case dets:lookup(RepoHandle, PlayerId) of
        [Player] -> {ok, Player};
        [] -> notfound
    end.

-spec merge(player:player(), RepoHandle::atom()) -> ok.
merge(Player, RepoHandle) ->
    ok = dets:insert(RepoHandle, Player),
    ok.

-spec close(RepoHandle::atom()) -> ok.
close(RepoHandle) ->
    ok = dets:close(RepoHandle).


%-spec award_achievement(PlayerId::binary(), achievement:achievement(), RepoHandle::atom()).
award_achievement(_State, PlayerId, AchievementName) ->
    OldPlayer = ensure_player(PlayerId),
    % FIXME: this should not be opened but must have the achievements repo as state
    {ok, Achievement} = achievements:load(AchievementName, achievements:open()),
    UpdatedPlayer = player:with_achievement(Achievement, OldPlayer),
    dets:insert(players, UpdatedPlayer),
    players_events:notify(players_events:achievement_award(UpdatedPlayer, Achievement)),
    ok.

-spec ensure_player(binary()) -> player:player().
ensure_player(PlayerId) ->
    case dets:lookup(players, PlayerId) of
        [Fetched] -> Fetched;
        [] ->
            ApiKey = google:api_key_make(element(2, application:get_env(google_api_key))),
            GoogleUser = google:user_profile(ApiKey, PlayerId),
            Player = user_to_player(GoogleUser),
            ok = dets:insert(players, Player),
            Player
    end.

-spec user_to_player(google:user()) -> player:player().
user_to_player(GoogleUser) ->
    player:new(google:user_id(GoogleUser), google:user_name(GoogleUser), google:user_image_url(GoogleUser)). 