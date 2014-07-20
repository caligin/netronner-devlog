-module(players_supervisor).
-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).

init([]) ->
    {ok, {
        { one_for_one, ?MAX_RESTART, ?MAX_TIME },
        [
            {players, {players, start_link, []}, permanent, brutal_kill, worker, [players]}
        ]
    }}.
