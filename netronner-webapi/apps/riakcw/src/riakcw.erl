-module(riakcw).
-export([
    start_named_link/3,
    start_named_link/4,
    endpoint/0
    ]).

-spec start_named_link(atom(), _Host, _Port) -> {ok,pid()}.
start_named_link(Name, Host, Port) ->
    start_named_link(Name, Host, Port, []).

-spec start_named_link(atom(), _Host, _Port, _Options) -> {ok,pid()}.
start_named_link(Name, Host, Port, Options) ->
    gen_server:start_link({local, Name}, riakc_pb_socket, [Host, Port, Options], []).

endpoint() ->
    {ok, Host} = application:get_env(riakcw, riak_host),
    {ok, Port} = application:get_env(riakcw, riak_port),
    {Host, Port}.

