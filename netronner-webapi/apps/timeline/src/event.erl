-module(event).
-export([make/2, type/1, ts/1, data/1, to_dto/1]).

-type event() :: { Type::binary(), Timestamp::erlang:timestamp(), Data::term()}.
-export_type([event/0]).

-spec make(binary(), term()) -> event().
make(Type, Data) ->
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

to_dto({Type, {Mega, Secs, Micro}, Data}) ->
    #{
        <<"type">> => Type,
        <<"timestamp">> => [Mega, Secs, Micro],
        <<"data">> => Data
    }.
