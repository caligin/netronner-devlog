-module(event).
-export([new/2, type/1, ts/1, data/1]).

-type event() :: { Type::binary(), Timestamp::erlang:timestamp(), Data::term()}.
-export_type([event/0]).

-spec new(binary(), term()) -> event().
new(Type, Data) ->
    {Type, os:timestamp(), Data}.

-spec type(event()) -> binary().
type({Type, _Ts, _D}) ->
    Type.

-spec ts(event()) -> erlang:timestamp().
ts({_T, Timestamp, _D}) ->
    Timestamp.

-spec data(event()) -> term().
data({_T, _Ts, Data}) ->
    Data.
