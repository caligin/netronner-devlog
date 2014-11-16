-module(netronner_application).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    register(netronner_application, self()),
    Protocol = application:get_env(netronner, protocol, https),
    Port = application:get_env(netronner, port, 8080),
    Acceptors = application:get_env(netronner, acceptors, 100),

    ok = players_events:add_handler(netronner_events_publisher),
    ok = players_events:add_handler(netronner_authorization_infector),

    {TimelineRepo} = open_repositories(),

    spawn(fun() -> netronner_authorization_infector:initialize_infection_list(TimelineRepo) end),

    Dispatcher = cowboy_router:compile([
        {'_', [
            {"/api/timeline/:page", netronner_handler_json, [TimelineRepo]},
            {"/api/players/:player_id", netronner_handler_json, [players, load]},
            {"/api/players/:player_id/award_achievement", netronner_handler_json, [players, award_achievement]},
            {"/api/achievements", netronner_handler_json, [achievements, list_or_set]}
        ]}
    ]),
    start_cowboy(Protocol, Port, Acceptors, Dispatcher).

stop(_State) ->
    ok.

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
    {ok, Timeline} = timeline:open(),
    {Timeline}.