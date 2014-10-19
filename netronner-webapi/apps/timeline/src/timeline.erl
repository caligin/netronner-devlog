-module(timeline).

-behaviour(gen_server).
-export([append/1, page/1]).
-export([page_to_dto/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-define(BUCKET, <<"timeline">>).
-define(LATEST, <<"latest">>).
-define(PAGE_SIZE, 20).

-type page() :: { Previous::integer() | none, [event:event()]}.


-spec start_link() -> {ok,pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec append(event:event()) -> ok.
append(Event) ->
    gen_server:call(?MODULE, {append, Event}).

-spec page(integer() | latest) -> page().
page(PageIndex) ->
    gen_server:call(?MODULE, {page, PageIndex}).

page_to_dto({Prev, Events}) ->
    #{
        <<"previous">> => Prev,
        <<"events">> => lists:map(fun event:to_dto/1, Events)
    }.

init([]) ->
    {ok, new_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({append, Event}, _From, State) ->
    {reply, append(State, Event), State};
handle_call({page, PageIndex}, _From, State) ->
    {reply, page(State, PageIndex), State};
handle_call(_Msg, _From, State) ->
    {reply, {error, badreqeust}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


new_state() ->
    #{}.

append(_State, Event) ->
    LatestObj = fetch_or_make_latest(),
    {Prev, Events} = binary_to_term(riakc_obj:get_value(LatestObj)),
    EventsNum = length(Events),
    if
        EventsNum < ?PAGE_SIZE ->
            NewLatestVal = term_to_binary({Prev, [Event | Events]}),
            NewNumberedLatestObj = riakc_obj:new(?BUCKET, integer_to_binary(next_prev(Prev)), NewLatestVal),
            NewLatestObj = riakc_obj:update_value(LatestObj, NewLatestVal),
            ok = riakc_pb_socket:put(whereis(timeline_riakc), NewNumberedLatestObj),
            ok = riakc_pb_socket:put(whereis(timeline_riakc), NewLatestObj),
            ok;
        true ->
            NewLatestVal = term_to_binary({next_prev(Prev), [Event]}),
            NewLatestObj = riakc_obj:update_value(LatestObj, NewLatestVal),
            ok = riakc_pb_socket:put(whereis(timeline_riakc), NewLatestObj),
            ok
    end.

next_prev(none) ->
    1;
next_prev(Prev) ->
    Prev + 1.

page(_State, latest) ->
    page_or_empty(?LATEST);
page(_State, none) ->
    throw(notfound);
page(_State, PageIndex) when is_integer(PageIndex) ->
    page_or_empty(integer_to_binary(PageIndex)).

page_or_empty(PageTerm) ->
    case riakc_pb_socket:get(whereis(timeline_riakc), ?BUCKET, PageTerm) of
        {ok, Fetched} -> binary_to_term(riakc_obj:get_value(Fetched));
        {error, notfound} -> {none, []}
    end.

-spec fetch_or_make_latest() -> riakc_obj:riakc_obj().
fetch_or_make_latest() ->
    case riakc_pb_socket:get(whereis(timeline_riakc), ?BUCKET, ?LATEST) of
        {ok, Latest} -> Latest;
        {error,notfound} -> 
            EmptyLatest = {none, []},
            EmptyLatestLocal = riakc_obj:new(?BUCKET, ?LATEST, term_to_binary(EmptyLatest)),
            {ok, EmptyLatestObj} = riakc_pb_socket:put(whereis(timeline_riakc), EmptyLatestLocal, [return_body]),
            EmptyLatestObj
    end.
