-module(achievements).

-behaviour(gen_server).
-export([list/0, set/1, load/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-type achievement() :: #{ name => binary(), description => binary(), icon => binary()}.
-export_type([achievement/0]).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec list() -> [achievement()].
% -----------------------------------------------------------------------------
list() ->
    gen_server:call(?MODULE, {list}).

% -----------------------------------------------------------------------------
-spec set([achievement()]) -> ok.
% -----------------------------------------------------------------------------
set(Achis) ->
    gen_server:call(?MODULE, {set, Achis}).

% -----------------------------------------------------------------------------
-spec load(AchievementName::binary()) -> {ok, achievement()} | notfound.
% -----------------------------------------------------------------------------
load(AchievementName) ->
    gen_server:call(?MODULE, {load, AchievementName}).

init([]) ->
    {ok, new_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({list}, _From, State) ->
    Achievements = list(State),
    {reply, Achievements, State};
handle_call({set, Achievements}, _From, State) ->
    {ok, NewState} = set(State, Achievements),
    {reply, ok, NewState};
handle_call({load, AchievementName}, _From, State) ->
    Result = load(State, AchievementName),
    {reply, Result, State};
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

set(State, NewAchievements) ->
    %% TODO: validation
    NewState = maps:from_list(lists:map(fun(Achievement) -> {maps:get(name, Achievement), Achievement} end, NewAchievements)),
    {ok, NewState}.

load(State, AchievementName) ->
    case maps:find(AchievementName, State) of
        {ok, Achievement} -> {ok, Achievement};
        error -> notfound
    end.