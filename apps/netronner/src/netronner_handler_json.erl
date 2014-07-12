-module(netronner_handler_json).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(HEADERS, [{<<"content-type">>, <<"application/json">>}]).



init(_Type, Req, [Feature, Action]) ->
    {Method, _} = cowboy_req:method(Req),
    {ok, Req, {Feature, Action, Method}}.

handle(Req, {players, <<"GET">>}) ->
    Response = [
    {[
        {name, <<"Caligin">> },
        {win, 5 },
        {loss, 6 }
    ]},{[
        {name, <<"Mindy">> },
        {win, 4 },
        {loss, 3 }
    ]},{[
        {name, <<"Blacktazz">> },
        {win, 2 },
        {loss, 1 }
    ]}],
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, undefined};

handle(Req, {player, <<"GET">>}) ->
    {Name, _} = cowboy_req:binding(name, Req),
    case Name of
        <<"Caligin">> -> Response = {[
            {name, <<"Caligin">> },
            {win, 5 },
            {loss, 6 },
            {ron, 4 },
            {tsumo, 1 },
            {riichi, 3 }
        ]};
        <<"Mindy">> -> Response = {[
            {name, <<"Mindy">> },
            {win, 4 },
            {loss, 3 }
        ]};
        <<"Blacktazz">> -> Response = {[
            {name, <<"Blacktazz">> },
            {win, 2 },
            {loss, 1 }
        ]};
    end,
    {ok, Req2} = cowboy_req:reply(200, ?HEADERS, jiffy:encode(Response), Req),
    {ok, Req2, undefined};
handle(Req, {_, Method }) ->
    {ok, Req2} = cowboy_req:reply(405, ?HEADERS, jiffy:encode({[{unsupported_method, Method}]}), Req),
    {ok, Req2, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.
