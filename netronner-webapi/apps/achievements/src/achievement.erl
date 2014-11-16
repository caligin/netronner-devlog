-module(achievement).

-export([new/3, name/1, description/1, icon/1, eq/2]).

-type achievement() :: { Name::binary(), Description::binary(), Icon::binary()}.
-export_type([achievement/0]).

-spec new(binary(), binary(), binary()) -> achievement().
new(Name, Description, Icon) ->
    {Name, Description, Icon}.

-spec name(achievement()) -> binary().
name({Name, _, _}) ->
    Name.

-spec description(achievement()) -> binary().
description({_, Description, _}) ->
    Description.

-spec icon(achievement()) -> binary().
icon({_, _, Icon}) ->
    Icon.

-spec eq(achievement(), achievement()) -> boolean().
eq({LhName, _, _}, {RhName, _, _}) ->
    LhName =:= RhName.
