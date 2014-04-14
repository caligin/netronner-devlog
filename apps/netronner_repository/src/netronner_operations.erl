-module(netronner_operations).
-export([new/0, list_players/1, find/2]).

-record(achievement, { title, description}).

-record(player, { name, image_url, wins, losses, achievements = []}).

-type player_type() :: #player{}.
-type state() :: [player_type()].
-export_type([player_type/0]).

% -----------------------------------------------------------------------------
-spec new() -> state().
% -----------------------------------------------------------------------------
new() ->
    [].

% -----------------------------------------------------------------------------
-spec list(state()) -> [player_type()].
% -----------------------------------------------------------------------------
list(State) ->
    State. %% same for now, but I'm sure the full record and the list handle will differ.


% -----------------------------------------------------------------------------
-spec find_player(state(), binary()) -> {true, player_type()} | {false, Name::binary()}.
% -----------------------------------------------------------------------------
find_player([Player|T], Name) ->
    case binary:match(Player#player.name, Name) of
        nomatch -> find(T, Name);
        _ -> {true, Player}
    end;
find([], Name) ->
    {false, Name}.

