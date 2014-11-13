-module(players).

-behaviour(gen_server).
-export([load/1, add/1, award_achievement/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec load(binary()) -> player:player().
load(PlayerId) ->
    gen_server:call(?MODULE, {load, PlayerId}).

-spec add(player:player()) -> ok.
add(Player) ->
    gen_server:call(?MODULE, {add, Player}).

-spec award_achievement(PlayerId::binary(), AchievementName::binary()) -> ok.
award_achievement(PlayerId, AchievementName) ->
    gen_server:call(?MODULE, {award_achievement, PlayerId, AchievementName }).


init([]) ->
    %%TODO: configurable filename
    dets:open_file(players,[]),
    {ok, new_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({load, PlayerId}, _From, State) ->
    {reply, load(State, PlayerId), State};
handle_call({add, Player}, _From, State) ->
    ok = add(State, Player),
    {reply, ok, State};
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


new_state() ->
    #{}.

load(State, PlayerId) ->
    dets:lookup(players, PlayerId).

add(_State, Player) ->
    dets:insert(players, Player).

award_achievement(_State, PlayerId, AchievementName) ->
    OldPlayer = ensure_player(PlayerId),
    {ok, Achievement} = achievements:load(AchievementName),
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
    player:make(google:user_id(GoogleUser), google:user_name(GoogleUser), google:user_image_url(GoogleUser)). 