-module(players_supervisor).
-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).

init([]) ->
    {Host, Port} = riakcw:endpoint(),
    {ok, {
        { one_for_one, ?MAX_RESTART, ?MAX_TIME },
        [
            {players_riakc, {riakcw, start_named_link, [players_riakc, Host, Port]}, permanent, 5000, worker, [riakcw]},
            {players_events, {players_events, start_link, []}, permanent, 5000, worker, [players_events]},
            {players, {players, start_link, []}, permanent, 5000, worker, [players]}
        ]
    }}.
