-module(achievements_supervisor).
-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).

init([]) ->
    {ok, {
        { one_for_one, ?MAX_RESTART, ?MAX_TIME },
        [
            {achievements_riakc, {achievements, start_riakc, []}, permanent, brutal_kill, worker, [achievements]},
            {achievements, {achievements, start_link, []}, permanent, brutal_kill, worker, [achievements]}
        ]
    }}.
