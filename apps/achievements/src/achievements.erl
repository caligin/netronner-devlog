-module(achievements).

-behaviour(gen_server).
-export([list/0, put/1, award/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-type achievement() :: #{ name => binary(), description => binary(), icon => binary()}.
-type achievements() :: #{ binary() => achievement()}.
-export_type([achievement/0]).

% -----------------------------------------------------------------------------
-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% -----------------------------------------------------------------------------
-spec list() -> [achievements()].
% -----------------------------------------------------------------------------
list() ->
    gen_server:call(?MODULE, {list}).

% -----------------------------------------------------------------------------
-spec put([achievement()]) -> ok.
% -----------------------------------------------------------------------------
put(Achis) ->
    gen_server:call(?MODULE, {put, Achis}).

% -----------------------------------------------------------------------------
-spec award([achievement()]) -> ok.
% -----------------------------------------------------------------------------
put(Achis) ->
    gen_server:call(?MODULE, {put, Achis}).


init([]) ->
    {ok, netronner_driver:new()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({list}, From, State) ->
    async_(From, State, fun() -> netronner_operations:list(State) end);
handle_call({find_player, Name}, From, State) ->
    async_(From, State, fun() -> netronner_operations:find_player(State, Name) end);
handle_call(_Msg, _From, State) ->
    {reply, {error, badreqeust}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

async_(To, State, Fun) -> spawn(fun() -> gen_server:reply(To, Fun()) end), {noreply, State}.
