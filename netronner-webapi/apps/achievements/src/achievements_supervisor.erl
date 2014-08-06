-module(achievements_supervisor).
-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).

init([]) ->
    {Host, Port} = riakcw:endpoint(),
    {ok, {
        { one_for_one, ?MAX_RESTART, ?MAX_TIME },
        [
            {achievements_riakc, {riakcw, start_named_link, [achievements_riakc, Host, Port]}, permanent, brutal_kill, worker, [riakcw]},
            {achievements, {achievements, start_link, []}, permanent, brutal_kill, worker, [achievements]}
        ]
    }}.
