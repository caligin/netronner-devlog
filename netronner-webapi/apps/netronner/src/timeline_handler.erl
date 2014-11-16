-module(timeline_handler).
-export([
    init/3,
    content_types_provided/2,
    timeline_page_json/2
    ]).

init(_Transport, Req, [Repository]) ->
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
    {upgrade, protocol, cowboy_rest, Req2, Repository}.

content_types_provided(Req, Repository) ->
    {[{<<"application/json">>, timeline_page_json}], Req, Repository}.

timeline_page_json(Req, Repository) ->
    PageIndex = page_index_binding(Req),
    Page = timeline_page_to_dto(timeline:page(PageIndex, Repository)),
    {jiffy:encode(Page), Req, Repository}.


page_index_binding(Req) ->
    {PageIndexBin, _} = cowboy_req:binding(page, Req),
    case PageIndexBin of
        <<"latest">> -> latest;
        _ -> binary_to_integer(PageIndexBin)
    end.

timeline_page_to_dto({_Page, Prev, Events}) ->
    #{
        <<"previous">> => Prev,
        <<"events">> => lists:map(fun event_to_dto/1, Events)
    }.

event_to_dto(Event) ->
    {Mega, Secs, Micro} = event:ts(event),
    #{
        <<"type">> => event:type(Event),
        <<"timestamp">> => [Mega, Secs, Micro],
        <<"data">> => event:data(Event)
    }.
