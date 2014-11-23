-module(netronner_application).

-behaviour(application).
-behaviour(supervisor).
-export([
    start/2,
    stop/1
    ]).
-export([
    init/1
    ]).

%% application cbs
start(_StartType, _StartArgs) ->
    register(netronner_application, self()),
    Protocol = application:get_env(netronner, protocol, https),
    Port = application:get_env(netronner, port, 8080),
    Acceptors = application:get_env(netronner, acceptors, 100),

    {TimelineRepo, AchievementsRepo, PlayersRepo} = open_repositories(),

    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/api/timeline/:page", timeline_handler, [TimelineRepo]},
            {"/api/players/:player_id", player_handler, [PlayersRepo]},
            {"/api/players/:player_id/achievements", player_achievements_handler, [PlayersRepo, AchievementsRepo, event_bus]},
            {"/api/achievements", achievements_handler, [AchievementsRepo]}
        ]}
    ]),
    {ok, _} = start_cowboy(Protocol, Port, Acceptors, Dispatcher),
    SupervisorRef = start_link_supervisor(),
    ok = gen_event:add_handler(event_bus, netronner_events_publisher, [TimelineRepo]),
    SupervisorRef.

stop(_State) ->
    ok.

%% supervisor cbs
init([]) ->
    {ok, {
        { one_for_one, 5, 100 },
        [{event_bus, {gen_event, start_link, [{local, event_bus}]}, permanent, 5000, worker, [dynamic]}]
    }}.

%% private
start_cowboy(http, Port, Acceptors, Dispatcher) ->
    cowboy:start_http(cowboy_ref, Acceptors, [{port, Port}], [ {env, [{dispatch, Dispatcher}]} ]);

start_cowboy(https, Port, Acceptors, Dispatcher) ->
    {ok, CaCertFile} = application:get_env(cacertfile),
    {ok, CertFile} = application:get_env(certfile),
    {ok, KeyFile} = application:get_env(keyfile),
    cowboy:start_https(cowboy_ref, Acceptors, [{port, Port},{cacertfile, CaCertFile},{certfile, CertFile},{keyfile, KeyFile}], [ {env, [{dispatch, Dispatcher}]} ]);

start_cowboy(UnsupportedProtocol, _, _, _) ->
    throw({unsupported_protocol, UnsupportedProtocol}).

open_repositories() ->
    {ok, BasePath} = application:get_env(db_base_path),
    {ok, Timeline} = timeline:open(BasePath),
    {ok, Achievements} = achievements:open(BasePath),
    {ok, Players} = players:open(BasePath),
    {Timeline, Achievements, Players}.

start_link_supervisor() ->
    supervisor:start_link({local, netronner_supervisor}, ?MODULE, []).