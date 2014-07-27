-module(players).

-behaviour(gen_server).
-export([list/0, add/1, award_achievement/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec list() -> [player:player()].
% -----------------------------------------------------------------------------
list() ->
    gen_server:call(?MODULE, {list}).

% -----------------------------------------------------------------------------
-spec add(player:player()) -> ok.
% -----------------------------------------------------------------------------
add(Player) ->
    gen_server:call(?MODULE, {add, Player}).

% -----------------------------------------------------------------------------
-spec award_achievement(PlayerId::binary(), AchievementName::binary()) -> ok.
% -----------------------------------------------------------------------------
award_achievement(PlayerId, AchievementName) ->
    gen_server:call(?MODULE, {award_achievement, PlayerId, AchievementName }).

init([]) ->
    {ok, new_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({list}, _From, State) ->
    {reply, list(State), State};
handle_call({add, Player}, _From, State) ->
    {ok, NewState} = add(State, Player),
    {reply, ok, NewState};
handle_call({award_achievement, PlayerId, AchievementName}, _From, State) ->
    {ok, NewState} = award_achievement(State, PlayerId, AchievementName),
    {reply, ok, NewState};
handle_call(_Msg, _From, State) ->
    {reply, {error, badreqeust}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

new_state() ->
    #{}.

list(State) ->
    maps:values(State).

add(State, Player) ->
    {ok, maps:put(player:id(Player), Player, State)}.

award_achievement(State, PlayerId, AchievementName) ->
    Player = maps:get(PlayerId, State),
    {ok, Achievement} = achievements:load(AchievementName),
    {ok, maps:put(PlayerId, player:with_achievement(Achievement, Player), State)}.
