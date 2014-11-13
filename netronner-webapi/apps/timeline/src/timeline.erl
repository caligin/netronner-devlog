-module(timeline).

-behaviour(gen_server).
-export([append/1, page/1]).
-export([page_to_dto/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0]).

-define(PAGE_SIZE, 20).

-type page() :: { Page::integer() | latest, Previous::integer() | none, [event:event()]}.


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
    %% TODO: configurable file name
    dets:open_file(events, []),
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
    ok = dets:close(events),
    ok.


new_state() ->
    #{}.

append(_State, Event) ->
    {latest, Prev, Events} = fetch_or_make_latest(),
    EventsCount = length(Events),
    if
        EventsCount < ?PAGE_SIZE ->
            NewLatest = {latest, Prev, [Event | Events]},
            NewNumberedLatest = {next_prev(Prev), Prev, [Event | Events]},
            ok = dets:insert(events, NewLatest),
            ok = dets:insert(events, NewNumberedLatest),
            ok;
        true ->
            NewLatest = {latest, next_prev(Prev), [Event]},
            ok = dets:insert(events, NewLatest),
            ok
    end.

next_prev(none) ->
    1;
next_prev(Prev) ->
    Prev + 1.

page(_State, none) ->
    throw(notfound);
page(_State, latest) ->
    page_or_empty(latest);
page(_State, PageIndex) when is_integer(PageIndex) ->
    page_or_empty(PageIndex).

page_or_empty(PageTerm) ->
    case dets:lookup(events, PageTerm) of
        [{_Page, Prev, Events}] -> {Prev, Events};
        [] -> {none, []}
    end.

-spec fetch_or_make_latest() -> page().
fetch_or_make_latest() ->
    case dets:lookup(events, latest) of
        [Latest] -> Latest;
        [] -> 
            EmptyLatest = {latest, none, []},
            ok = dets:insert(events, EmptyLatest),
            EmptyLatest
    end.
