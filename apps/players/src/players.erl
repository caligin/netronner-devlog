-module(players).

-behaviour(gen_server).
-export([list/0, add/1, award_achievement/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-type player() :: #{ name => binary(), icon => binary(), achievements => [achievement()]}.
-export_type([player/0]).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec list() -> [player()].
% -----------------------------------------------------------------------------
list() ->
    gen_server:call(?MODULE, {list}).

% -----------------------------------------------------------------------------
-spec put(player()) -> ok.
% -----------------------------------------------------------------------------
add(Player) ->
    gen_server:call(?MODULE, {add, Player}).

% -----------------------------------------------------------------------------
-spec award_achievement(PlayerName::binary(), AchievementName::binary()) -> ok.
% -----------------------------------------------------------------------------
award_achievement(PlayerName, AchievementName) ->
    gen_server:call(?MODULE, {award_achievement, Player, Achievement }).


init([]) ->
    {ok, new_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({list}, _From, State) ->
    {reply, list(State), State};
handle_call({add, Player}, _From, State) ->
    {ok, NewState} = add(State, Player),
    {reply, ok, NewState};
handle_call({award_achievement, PlayerName, AchievementName}, _From, State) ->
    {ok, NewState} = award_achievement(State, PlayerName, AchievementName),
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
    PlayerName = maps:get(name, Player),
    {ok, maps:put(PlayerName, Player, State)}.

award_achievement(State, PlayerName, AchievementName) ->
    Player = maps:get(PlayerName, State),
    {ok, Achievement} = achievements:load(AchievementName),
    PlayerAchis = maps:get(achievements, Player),
    Updated = maps:put(achievements, [Achievement | PlayerAchis], Player),
    {ok, maps:put(PlayerName, Updated, State)}.
