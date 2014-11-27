-module(timeline).
-export([
    open/0,
    open/1,
    append/2,
    page/2,
    close/1
    ]).
-define(PAGE_SIZE, 20).

-type page() :: { Page::integer() | latest, Previous::integer() | none, [event:event()]}.

-spec open() -> {ok, RepoHandle::atom()}.
open() ->
    dets:open_file(?MODULE,[]).

-spec open(BasePath::string()) -> {ok, RepoHandle::atom()}.
open(BasePath) ->
    dets:open_file(?MODULE,[{file, [BasePath, $/, atom_to_list(?MODULE)]}]).

-spec append(event:event(), RepoHandle::atom()) -> ok.
append(Event, RepoHandle) ->
    {latest, Prev, Events} = fetch_or_make_latest(RepoHandle),
    EventsCount = length(Events),
    if
        EventsCount < ?PAGE_SIZE ->
            NewLatest = {latest, Prev, [Event | Events]},
            NewNumberedLatest = {next_prev(Prev), Prev, [Event | Events]},
            ok = dets:insert(RepoHandle, NewLatest),
            ok = dets:insert(RepoHandle, NewNumberedLatest),
            ok;
        true ->
            NewLatest = {latest, next_prev(Prev), [Event]},
            ok = dets:insert(RepoHandle, NewLatest),
            ok
    end.

next_prev(none) ->
    1;
next_prev(Prev) ->
    Prev + 1.

-spec page(integer() | latest | none, RepoHandle::atom()) -> page().
page(none, _RepoHandle) ->
    throw(notfound);
page(latest, RepoHandle) ->
    page_or_empty(latest, RepoHandle);
page(PageIndex, RepoHandle) when is_integer(PageIndex) ->
    page_or_empty(PageIndex, RepoHandle).

page_or_empty(PageTerm, RepoHandle) ->
    case dets:lookup(RepoHandle, PageTerm) of
        [Page] -> Page;
        [] -> {latest, none, []}
    end.

-spec fetch_or_make_latest(RepoHandle::atom()) -> page().
fetch_or_make_latest(RepoHandle) ->
    case dets:lookup(RepoHandle, latest) of
        [Latest] -> Latest;
        [] -> 
            EmptyLatest = {latest, none, []},
            ok = dets:insert(RepoHandle, EmptyLatest),
            EmptyLatest
    end.

-spec close(RepoHandle::atom()) -> ok | {error, term()}.
close(RepoHandle) ->
    dets:close(RepoHandle).
