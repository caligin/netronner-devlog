-module(players).

-behaviour(gen_server).
-export([list/0, add/1, award_achievement/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-define(BUCKET, <<"players">>).

-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec list() -> [player:player()].
list() ->
    gen_server:call(?MODULE, {list}).

-spec add(player:player()) -> ok.
add(Player) ->
    gen_server:call(?MODULE, {add, Player}).

-spec award_achievement(PlayerId::binary(), AchievementName::binary()) -> ok.
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

add(_State, Player) ->
    Obj = riakc_obj:new(?BUCKET, player:id(Player), term_to_binary(Player)),
    ok = riakc_pb_socket:put(whereis(players_riakc), Obj),
    {ok, _State}.

award_achievement(_State, PlayerId, AchievementName) ->
    OldObj = ensure_player(PlayerId),
    {ok, Achievement} = achievements:load(AchievementName),
    OldPlayer = binary_to_term(riakc_obj:get_value(OldObj)),
    UpdatedPlayer = player:with_achievement(Achievement, OldPlayer),
    NewObj = riakc_obj:update_value(OldObj, term_to_binary(UpdatedPlayer)),
    riakc_pb_socket:put(whereis(players_riakc), NewObj),
    {ok, _State}.

-spec ensure_player(binary()) -> riakc_obj:riakc_obj().
ensure_player(PlayerId) ->
    case riakc_pb_socket:get(whereis(players_riakc), ?BUCKET, PlayerId) of
        {ok, Fetched} -> Fetched;
        {error,notfound} ->
            ApiKey = google:api_key_make(element(2, application:get_env(google_api_key))),
            GoogleUser = google:user_profile(ApiKey, PlayerId),
            Player = user_to_player(GoogleUser),
            Obj = riakc_obj:new(?BUCKET, player:id(Player), term_to_binary(Player)),
            {ok, NewObj} = riakc_pb_socket:put(whereis(players_riakc), Obj, [return_body]),
            NewObj
    end.

-spec user_to_player(google:user()) -> player:player().
user_to_player(GoogleUser) ->
    player:make(google:user_id(GoogleUser), google:user_name(GoogleUser), google:user_image_url(GoogleUser)). 