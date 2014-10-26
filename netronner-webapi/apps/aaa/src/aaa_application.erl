-module(aaa_application).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 100).

start(_StartType, _StartArgs) ->
   register(aaa_application, self()),
   supervisor:start_link({local, aaa_supervisor}, aaa_application, []).

stop(_State) ->
    ok.


init([]) ->
    {ok, {
        { one_for_one, ?MAX_RESTART, ?MAX_TIME },
        [
            {google_viral_authorization, {google_viral_authorization, start_link, []}, permanent, 5000, worker, [google_viral_authorization]}
        ]
    }}.
