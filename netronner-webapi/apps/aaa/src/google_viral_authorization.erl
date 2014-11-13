-module(google_viral_authorization).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([is_infected/1, infect/1]).

%% Caligin Tsukihara's google id.
%% TODO: configurable seed?
-define(SEED, <<"108105329232958151930">>).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

is_infected(GoogleId) ->
    gen_server:call(?MODULE, {is_infected, GoogleId}).

infect(GoogleId) ->
    gen_server:call(?MODULE, {infect, GoogleId}).

init(_Args) ->
    {ok, sets:from_list([?SEED])}.

handle_call({is_infected, GoogleId}, _From, State) ->
    {reply, sets:is_element(GoogleId, State), State};
handle_call({infect, GoogleId}, _From, State) ->
    {reply, ok, sets:add_element(GoogleId, State)};
handle_call(_Request, _From, State) ->
    {reply, {error, badreqeust}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
