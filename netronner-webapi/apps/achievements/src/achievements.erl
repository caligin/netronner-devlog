-module(achievements).

-behaviour(gen_server).
-export([list/0, set/1, load/1]).
-export([from_json/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-define(BUCKET, <<"achievements">>).
-define(KEY, <<"achievements">>).


-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec list() -> [achievement:achievement()].
list() ->
    gen_server:call(?MODULE, {list}).

-spec set([achievement:achievement()]) -> ok.
set(Achis) ->
    gen_server:call(?MODULE, {set, Achis}).

-spec load(AchievementName::binary()) -> {ok, achievement:achievement()} | notfound.
load(AchievementName) ->
    gen_server:call(?MODULE, {load, AchievementName}).

-spec from_json(JsonAchievements::binary()) -> [achievement:achievement()].
from_json(JsonAchievements) ->
    BinaryAchis = jiffy:decode(JsonAchievements, [return_maps]),
    decoded_to_achievements(BinaryAchis, []).

decoded_to_achievements([BinaryAchievement | Others], Converted) ->
    Name = maps:get(<<"name">>, BinaryAchievement),
    Description = maps:get(<<"description">>, BinaryAchievement),
    Icon = maps:get(<<"icon">>, BinaryAchievement),
    Achievement = achievement:make(Name, Description, Icon),
    decoded_to_achievements(Others, [Achievement | Converted]);
decoded_to_achievements([], Converted) ->
    Converted.

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
    {reply, {error, badrequest}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


new_state() ->
    #{}.

list(_State) ->
    Achievements = fetch_document(),
    maps:values(Achievements).

set(_State, NewAchievements) ->
    %% TODO: validation
    NewState = maps:from_list(lists:map(fun(Achievement) -> {achievement:name(Achievement), Achievement} end, NewAchievements)),
    Obj = riakc_obj:new(?BUCKET, ?KEY, term_to_binary(NewState)),
    ok = riakc_pb_socket:put(whereis(achievements_riakc), Obj),
    {ok, NewState}.

load(_State, AchievementName) ->
    case maps:find(AchievementName, fetch_document()) of
        {ok, Achievement} -> {ok, Achievement};
        error -> notfound
    end.

fetch_document() ->
    case riakc_pb_socket:get(whereis(achievements_riakc), ?BUCKET, ?KEY) of
        {ok, Fetched} -> binary_to_term(riakc_obj:get_value(Fetched));
        {error,notfound} -> #{}
    end.
