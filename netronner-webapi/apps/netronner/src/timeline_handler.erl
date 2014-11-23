-module(timeline_handler).
-export([
    init/3,
    rest_init/2,
    content_types_provided/2,
    timeline_page_json/2
    ]).

init(_Transport, Req, [Repository]) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {upgrade, protocol, cowboy_rest, Req2, Repository}.

rest_init(Req, Repository) ->
    {ok, Req, Repository}.    

content_types_provided(Req, Repository) ->
    {[{<<"application/json">>, timeline_page_json}], Req, Repository}.

timeline_page_json(Req, Repository) ->
    PageIndex = page_index_binding(Req),
    Page = encode_json(timeline:page(PageIndex, Repository)),
    {Page, Req, Repository}.


page_index_binding(Req) ->
    {PageIndexBin, _} = cowboy_req:binding(page, Req),
    case PageIndexBin of
        <<"latest">> -> latest;
        _ -> binary_to_integer(PageIndexBin)
    end.

encode_json({_Page, Prev, Events}) ->
    AsMap = #{
        <<"previous">> => Prev,
        <<"events">> => lists:map(fun event_to_dto/1, Events)
    },
    jiffy:encode(AsMap).

event_to_dto(Event) ->
    {Mega, Secs, Micro} = event:ts(Event),
    #{
        <<"type">> => event:type(Event),
        <<"timestamp">> => [Mega, Secs, Micro],
        <<"data">> => event:data(Event)
    }.
